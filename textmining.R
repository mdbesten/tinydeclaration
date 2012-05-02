#    Copyright: Matthijs den Besten
#
#    This file is part of "tinydeclaration".
#
#    "tinydeclaration" is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    "tinydeclaration" is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with "tinydeclaration".  If not, see <http://www.gnu.org/licenses/>.
#

library(chron);
# ET - (UTC - 5)
slate.pubs <- data.frame(title =
                         c("A Slate Twitter contest for the Fourth of July",
                           "All Men Are Tweeted Equal (update)",
                           "Life, Liberty, and 140 Characters"),
                         date =
                         chron(dates =
                               c("06/28/10",
                                 "06/30/10",
                                 "07/02/10"),
                               times =
                               c("23:00:00",
                                 "14:45:00",
                                 "22:16:00")));
                                        
read.all <- function() {
  twit.tab <- read.tweets();
  twit.old <- read.tweets(read.csv("FirstDeclarations.csv"));
  twit.all <- rbind(twit.tab, twit.old[!twit.old$date%in%twit.tab$date,]);
  twit.all <- twit.all[order(twit.all$date),];
  return(twit.all);
}

read.tweets <- function(tab = read.csv("TinyDeclaration.csv")) {
                                        # convert timestamp to chron date
                                        # twitter reports UTC
  date.time <- strsplit(sub(" [+].[0-9]*$","",
                            sub("^ ", "",
                                sub("2010 ", "2010;",
                                    tab$timestamp))),
                        ";");
  tab[,"date"] <- chron(sapply(date.time, head, 1),
                        sapply(date.time, tail,1),
                        format=list(dates="day mon year", times="hh:mm:ss"),
                        out.format=list(dates="d/m/y", times="hh:mm:ss"));

                                        # clean strings
  tab[,"tweet"] <- gsub("COMMA", ",", as.character(tab$title));
                                        # TODO: remove #TinyDeclaration[: ]

  return(tab[,c("author", "date", "tweet")]);
}

table2Corpus <- function(tab = read.tweets(),
                         mapping = list(Content = "tweet",
                           Author = "author", Date = "date"),
                         cleanup = TRUE) {
  if(cleanup) {
    replaceHTML <- function(str) {
      return(gsub("&quot;", '"',
                  gsub("&amp;", "&",
                       gsub("&lt;", "<",
                            gsub("&gt;", ">",
                                 str)))));
    }
    tab$tweet <- sapply(tab$tweet, replaceHTML);
    
  }
  
  require(tm);
  myReader <- readTabular(mapping = mapping);
  return(Corpus(DataframeSource(tab), readerControl=list(reader = myReader))); 
}


is.retweet <- function(tweet) {
  return(grepl("RT ", tweet));
}

has.url <- function(tweet) {
  return(grepl("://", tweet));
}

is.submission <- function(tab) {
  return(tab$date < slate.pubs$date[3] &
         !is.retweet(tab$tweet) &
         !has.url(tab$tweet));
}

list.addressees <- function(tweet) {
  return(sapply(sapply(tweet, strsplit, "@"),
                function(v) {
                  return(sapply(sapply(v[-1],
                                       strsplit, "[[:punct:][:space:]]"),
                                head, 1))}));
}


#retweeted <- sapply(which(is.retweet(twit.all$tweet)), function(i) which.nearby(twit.all[i,], twit.all))
which.nearby <- function(ref, tab = twit.all, method = "cosine") {
  require(tm);
  removeURLs <- function(str) {
    return(gsub("http://[^[:space:]]*", "", str));
  }
  replaceHTML <- function(str) {
    return(gsub("&quot;", '"',
                gsub("&amp;", "&",
                     gsub("&lt;", "<",
                          gsub("&gt;", ">",
                               str)))));
  }
  removeTopic <- function(str) {
    return(gsub("#[^[:space:]]*", "", str));
  }

  get.addressees <- function(str) {
    return(sapply(strsplit(str, "@"),
           function(v) {
             return(sapply(sapply(v[-1],
                                  strsplit, "[[:punct:][:space:]]"),
                           head, 1));
           }));
  }
  
  candidate.tweets <- function(ref, tab) {
    addressees <- get.addressees(ref$tweet);
    if(length(addressees) > 0) {
      return(which(tab$date < ref$date &
                   tolower(tab$author) %in% tolower(addressees)));
    } else {
      return(vector());
    }
  }
  
  my.candidates <- candidate.tweets(ref, tab);
  if(length(my.candidates) > 0) {
    my.tweets <- c(ref$tweet, tab$tweet[my.candidates]);
    
    my.tweets <- sapply(sapply(sapply(my.tweets, removeURLs),
                               replaceHTML),
                        removeTopic);
    
    myCorpus <- Corpus(DataframeSource(data.frame(my.tweets)));
    myCorpus <- tm_map(tm_map(myCorpus, stripWhitespace), removeWords, "RT");
    
    distances <- sapply(2:length(myCorpus),
                        function(i) {
                          return(dissimilarity(myCorpus[[1]],
                                               myCorpus[[i]],
                                               method = method));
                        });
    choice <- my.candidates[which.min(distances)];
    dist <- min(distances);
    names(dist) <- choice;
    return(dist);
  } else {
    return(NA);
  }
}

#     tdm <- TermDocumentMatrix(crude)
 #    dissimilarity(tdm, method = "cosine")

ecdf.tweet <- function() {
  require(latticeExtra);
  twit.tab <- read.tweets();
  twit.old <- read.tweets(read.csv("FirstDeclarations.csv"));
  twit.all <- rbind(twit.tab, twit.old[!twit.old$date%in%twit.tab$date,]);
  twit.all <- twit.all[order(twit.all$date),];
  myplot <- asTheEconomist(ecdfplot(as.numeric(twit.all$date),
                                    groups =
                                    ifelse(is.retweet(twit.all$tweet) |
                                           has.url(twit.all$tweet),
                                           "retweet", "submission"),
                                    auto.key=list(columns=2),
                                    scales=list(x=list(at=14789:14795,
                                                  labels=chron(14789:14795,
                                                    out.format="d/m/y")))));

  return(myplot +
         layer_(panel.text(slate.pubs$date, 1.05,
                           substr(slate.pubs$title, 1, 24), cex=.6)) +
         layer_(panel.abline(v=slate.pubs$date, lty=3, cex=.5)));
}

# src: row in tab where tweet can be found
# TODO: why does src sometimes refer to retweet?
# score: number of retweets + 1
# topscore: max number of retweets received overall + 1
tweet.regard <- function(rt = which(is.retweet(twit.all$tweet)),
#                           & ! has.url(twit.all$tweet)), # good idea?
                         rt.src = sapply(rt,
                           function(i) which.nearby(twit.all[i,], twit.all)),
                         tab = twit.all) {
  selection <- !is.na(rt.src) & rt.src < 0.4;
  rt.src.row <- as.numeric(names(rt.src[selection]));
  src.tab <- tab[unique(rt.src.row),];
  rt.tab <- tab[rt[selection],];

  regard <- rbind(src.tab, rt.tab);
  regard[,"src"] <- c(unique(rt.src.row), rt.src.row);
  regard[,"score"] <- unsplit(sapply(split(regard$date, regard$src),
                                     function(v) 1:length(v)),
                              regard$src);
  regard[,"topscore"] <- unsplit(sapply(split(regard$score, regard$src),
                                         max),
                                  regard$src);
  return(regard);
}

regard.plot <- function(df = tweet.regard()) {
  myplot <- xyplot((score-1) ~ as.numeric(date), groups=src,
                   df, type="l", ylab="Total Retweets",
                   scales=list(x=list(at=14789:14795,
                                 labels=chron(14789:14795,
                                   out.format="d/m/y"))));
  podium <- df$src[df$score==1&df$topscore>5];
  
  return(myplot +
         layer_(panel.text(slate.pubs$date, max(df$score[df$topscore<90]),
                           substr(slate.pubs$title, 1, 24), cex=.6)) +
         layer_(panel.abline(v=slate.pubs$date, lty=3, cex=.5)) +
         layer_(panel.text(sapply(podium,
                                  function(x) {
                                    df$date[df$src==x&df$score==df$topscore];
                                  }),
                           sapply(podium,
                                  function(x) {
                                    df$score[df$src==x&df$score==df$topscore];
                                  }),
                           sapply(podium,
                                  function(x) {
                                    df$author[df$src==x&df$score==1];
                                  }))));
}

# sr <- summary.regard(tr)[,-3]
# sr <- sr[order(sr$date),]
# activities <- cbind(sr[,-5], t(sapply(as.character(sr$author), summary.author)))
summary.regard <- function(df = tweet.regard(), min.regard = 5) {
   podium <- df$src[df$score==1 & df$topscore>min.regard];

   foobar <- function(v, col) {
     return(sapply(v, function(x) {
       return(col[df$src == x & df$score == 1]);
     }));
   }

   tab <- data.frame(author = foobar(podium, as.character(df$author)),
                     date = foobar(podium, df$date),
                     tweet = foobar(podium, df$tweet),
                     topscore = foobar(podium, df$topscore)-1,
                     prescore = sapply(podium,
                       function(x) {
                         scores <- df$score[df$src == x &
                                            df$date < slate.pubs$date[3]];
                         return(ifelse(length(scores > 0),
                                       max(scores)-1,
                                       0));
                       }),
                     supporters = sapply(podium,
                       function(x) {
                         return(length(unique(df$author[df$src == x]))-1);
                       }));
   return(tab);
}

summary.author <- function(author = "RennaW",
                           tab = twit.all[twit.all$date<slate.pubs$date[3],]){
  author.tweets <- tab$tweet[tab$author==author];
  n <- length(author.tweets);
  n.rt <- sum(is.retweet(author.tweets));
  n.url <- sum(has.url(author.tweets));

  audience <- tab$author[grepl(tolower(author), tolower(tab$tweet)) &
                         is.retweet(tab$tweet)];
  return(c(tweet = n - n.rt, retweet = n.rt, url = n.url,
           supporters = length(unique(audience)),
           compliments = length(audience)));
}

#submission.rows <- twit.all$date<slate.pubs$date[3]&!is.retweet(twit.all$tweet)&!has.url(twit.all$tweet)
#submission.dates <- twit.all$date[submission.rows];
#submission.rows <- order(submission.dates)
#submission.dates <- sort(submission.dates)
#submission.addressees <- addressee.list[submission.rows]
#submission.panels <- cut(submission.rows, 6)

addressee.frame <- function(tab = read.all()) {
  tab <- tab[!is.retweet(tab$tweet) & !has.url(tab$tweet),];
  addr <- list.addressees(tab$tweet);
  addrptweet <- sapply(addr, length);
  
  return(data.frame(addressee = unlist(addr),
                    date = rep(tab$date, addrptweet)));
}

addressee.group <- function(name = addressee.frame()$addressee) {
  my.groups <- ifelse(grepl("slate", name, ignore.case=TRUE),
                      "Slate",
                      ifelse(grepl("georg[i]?e", name, ignore.case=TRUE),
                             "George",
                             ifelse(grepl("locke", name, ignore.case=TRUE),
                                    "Locke",
                                    ifelse(grepl("hancock", name, ign = TRUE),
                                           "Hancock",
                                           ifelse(grepl("kg", name, ig=TRUE),
                                                  "KG",
                                                  "other")))));
  npg <- summary(factor(my.groups));
  npg <- npg[c("KG", "Slate", "other", "George", "Locke", "Hancock")];
  group.labels <- paste(names(npg), " (", npg, ")", sep = "");
    
  return(factor(my.groups,
                levels = names(npg), labels = group.labels));
}

addressee.density <- function(af = addressee.frame(),
                              ticks = seq(14790, 14795, 2)) {
  myplot <-  densityplot(~ date | addressee.group(af$addressee), af,
                         ylim=c(0,.8), xlim=range(af$date),
                         scales=list(x=list(at=ticks,
                                       labels=chron(ticks,
                                         out.format="d/m/y"))));

  return(myplot +
         layer_(panel.abline(v=slate.pubs$date, lty=3, cex=.5)));
}


foobar <- function(twits = read.all()) {
  twit.corpus <- table2Corpus(twits[!(is.retweet(twits$tweet)|has.url(twits$tweet)),]);
  require(RWeka);
  dtm2 <- DocumentTermMatrix(twit.corpus, control=list(tokenizer=NGramTokenizer))
  dtm.sub <- dtm2[,findFreqTerms(dtm2,2)]

  pc <- princomp(dtm.sub);
  selection <- !(is.retweet(twits$tweet)|has.url(twits$tweet));
  regimes <- cut(twits$date[selection], c(slate.pubs$date, max(twits$date)),
                 labels=slate.pubs$title)

  regimes <- factor(as.character(regimes), levels=rev(levels(regimes)))
  rt <- which(is.retweet(twits$tweet));
  rt1 <- sapply(rt, function(i)
                which.nearby(twits[i,], twits))

  my.rows <- which(selection)
  no.retweets <- sapply(split(rt1, names(rt1)), length)
  my.retweets <- numeric(sum(selection))
  names(my.retweets) <- my.rows;
  my.retweets[names(no.retweets)[names(no.retweets)%in%my.rows]] <- no.retweets[names(no.retweets)%in%my.rows];
  capped.rts <- ifelse(my.retweets>5, 6, my.retweets);
  require(lattice);
  require(latticeExtra);
  myplot <- xyplot(Comp.1 ~ Comp.2 | regimes, groups = capped.rts, data.frame(pc$scores[,1:2]), layout=c(1,3), auto.key=list(columns=7))
  return(myplot + layer_(panel.grid(v=-1, h=-1)))
}

################################################################################
# 10/05/11
# compute formality score
# (noun frequency + adjective freq. + preposition freq. + article freq. - pronoun freq.- verb freq. - adverb freq. - interjection freq. + 100)/2
# Formality of Language: definition,measurement and behavioral determinants
# FRANCIS HEYLIGHEN* & JEAN-MARC DEWAELE**
# tagPOS(myC[5])
# require(openNLP) install.packages("openNLPmodels.en")
# twits <- read.all()
# tab <- twits[!(is.retweet(twits$tweet)|has.url(twits$tweet)),]
# tab$tweet <- clean.tweets(tab$tweet)
# myC <- table2Corpus(tab)
# tr '/' '@' < str.txt | sed -e 's/[^@]*@\([A-Z]*\)/\1 /g'

# noun: NN, NNS, NP, NPS
# adjective: JJ, JJR, JJS
# preposition: IN
# article: WDT, DT, (PDT, CD)
# pronoun: PP, PP$, WP, WP$
# verb: VB, VBD, VBG, VBN, VBP, VBZ
# adverb: RB, RBR, RBS
# interjection: UH

# 
#Error in .jcast(getModel(model), "opennlp.maxent.MaxentModel") : 
#  connot cast anything but Java objects
#> tab$tweet[563]
#[1] "King&amp;parliament, your acts are intolerable. Attempts to negotiate equal treatment as British citizens have failed. Goodbye. #tinydeclaration"

# formality <- scan("f.txt")
get.formality <- function(tab = read.all()) {
  formality <- function(tweet = myC[[1]]) {
    require(openNLP);
    pos <- tagPOS(tweet);
    v <- sapply(strsplit(pos, "/"), function(x) sub(" .*","", x))[-1];
    
    nouns <- sum(v %in% c("NN", "NNS", "NP", "NPS"));
    adjectives <- sum(v %in% c("JJ", "JJR", "JJS"));
    prepositions <- sum(v == "IN");
    articles <- sum(v %in% c("WDT", "DT"));
    pronouns <- sum(v %in% c("PP", "PP$", "WP", "WP$"));
    verbs <- sum(v %in% c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"));
    adverbs <- sum(v %in% c("RB", "RBR", "RBS"));
    interjections <- sum(v == "UH");
    
    return((nouns + adjectives + prepositions + articles -
            pronouns - verbs - adverbs - interjections + 100) / 2);
  }
  
  clean.tweets <- function(str) {
    removeURLs <- function(str) {
      return(gsub("http://[^[:space:]]*", "", str));
    }
    replaceHTML <- function(str) {
      return(gsub("&quot;", '"',
                  gsub("&amp;", "&",
                       gsub("&lt;", "<",
                            gsub("&gt;", ">",
                                 str)))));
    }
    removeTopic <- function(str) {
      return(gsub("#[^[:space:]]*", "", str));
    }
    removeAddressee <- function(str) {
      return(gsub("@[^[:space:]]*", "", str));
    }
    
    return(removeURLs(replaceHTML(removeTopic(removeAddressee(str)))));
  }

  tab$tweet <- clean.tweets(tab$tweet);
  myC <- table2Corpus(tab);
  return(sapply(1:length(myC),
                function(i) {
                  f <- formality(myC[[i]]);
                  cat(paste(f, "\n"), file="f.txt", append=TRUE);
                  return(f);
                }));
}

# sometime:
# construct formality time series
# decompose/stl

################################################################################
# JoSIT

# todo: apply tolower to whole corpus
dis.matrix <- function(corpus, method) {
  n <- length(corpus);
  dis.mat <- matrix(0, n, n);
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      dis.mat[i,j] <- dissimilarity(corpus[[i]], corpus[[j]], method);
      dis.mat[j,i] <- dis.mat[i,j];
    }
  }
  rownames(dis.mat) <- colnames(dis.mat) <- names(corpus);
  return(dis.mat);
}

# return order according to first principal component
order.pc <- function(corpus, n = 5, comp=1) {
  require(RWeka);
  dtm <- DocumentTermMatrix(corpus,
                            control=list(tokenizer=NGramTokenizer))
  dtm.sub <- dtm[,findFreqTerms(dtm,n)]

  pc <- princomp(dtm.sub);
  return(order(pc$scores[,comp]));
}

# map: x-axis - chronologically y-axis - pc1

dis.frame <- function(corpus, tab, method) {
  id <- names(corpus);
  difra <- expand.grid(a=id, b=id);
  subtab <- tab[rownames(tab) %in% id,];
  difra[,"xa"] <- order(subtab$date)[match(difra$a,rownames(subtab))];
  difra[,"xb"] <- order(subtab$date)[match(difra$b,rownames(subtab))];
  difra[,"ya"] <- order.pc(corpus, comp=1)[match(difra$a, id)];
  difra[,"yb"] <- order.pc(corpus, comp=1)[match(difra$b, id)];
  difra[,"yya"] <- order.pc(corpus, comp=2)[match(difra$a, id)];
  difra[,"yyb"] <- order.pc(corpus, comp=2)[match(difra$b, id)];
  zmat <- dis.matrix(corpus, method);
  difra[,"z"] <- apply(difra, 1,
                       function(v) {
                         return(zmat[as.character(v[1]),as.character(v[2])]);
                       });
  return(difra);
}

#source("textmining.R")
#tab <- read.all()
#corp <- table2Corpus(tab[!(is.retweet(tab$tweet)|has.url(tab$tweet)),])
#sel100 <- sample(1:length(corp), 100)
#cos <- dis.frame(corp[sel100], tab, "cosine")
#library(lattice); library(latticeExtra)
#pdf("fig1.pdf")
#asTheEconomist(levelplot(z ~ xa * xb, cos, cuts=5, main="Cosine dissimilarity")#)
#dev.off()


# how many unique ideas?
unique.ideas <- function(cutoff=0.5, dima) {
  n = nrow(dima);
  uv <- 1:n;
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      if(dima[i,j] <= cutoff) {
        uv[j] <- i;
      }
    }
  }
  return(length(unique(uv)))
}


submat <- function(mat, subset) {
  if(length(intersect(rownames(mat), subset)) > 1) {
    smat <- mat[rownames(mat)%in%subset,];
    smat <- smat[,colnames(mat)%in%subset];
  } else {
    return(matrix(0, 0, 0));
  }

  return(smat);
}

#sapply(seq(0,0.5,0.1), unique.ideas, submat(cosmat,phase1)
#phase1 <- rownames(tab)[tab$date>slate.pubs$date[1]&tab$date<slate.pubs$date[2]]
dawkins91 <- function(u, n) {
  return(u^2/(2*(n-u)));
}

dendro.neighbors <- function(mat, slice) {
  v <- cutree(hclust(as.dist(mat)), h=1-slice);
  w <- sapply(split(names(v), v), length);
  vv <- w[v];
  names(vv) <- names(v);

  return(vv);
}

neighbors.around <- function(mat, horizon) {
  return(apply(mat, 1, function(x) sum(x<=horizon)-1));
}

peer.approval <- function(twr = tweet.regard(), cutoff=slate.pubs$date[3],
                          labels = rownames(tab)) {
  twrsub <- twr[twr$date<cutoff,];
  v <- sapply(split(twrsub$score, twrsub$src), tail, 1)-1;
  names(v) <- labels[as.numeric(names(v))];

  return(v);
}

# always 1 per review
unique.peers <- function(twr = tweet.regard(), cutoff=slate.pubs$date[3],
                         labels = rownames(tab)) {
  twrsub <- twr[twr$date<cutoff,];
  v <- sapply(split(twrsub$author,twrsub$src, drop = TRUE), length) - 1;
  names(v) <- labels[as.numeric(names(v))];

  return(v);
}

tweet.age <- function(twr, cutoff = slate.pubs$date[3]) {
  return(cutoff-twr$date[twr$score==1]);
}

# extensive frame
extensive.frame <- function(tab = twit.all,
                            twr = tweet.regard(),
                            mat = cosmat,
                            fun = neighbors.around,
                            par = 0.3,
                            cutoff = slate.pubs$date[3]) {
  smat <- submat(mat,rownames(tab)[tab$date<cutoff]);
  id <- rownames(smat);
  pa <- peer.approval(twr);
  v <- numeric(length(id));
  names(v) <- id;
  # bug (?): names all pa tweets in smat
  pasel <- intersect(names(pa),id);
  v[pasel] <- pa[pasel];
  is.retweeted <- rownames(tab)[twr$src[twr$score==1]];
  return(data.frame(id = id,
                    has.retweet = id %in% is.retweeted,
                    approval = v,
                    age = cutoff-tab[id,]$date,
                    naa = neighbors.at.arrival(smat, tab, par),
                    paa = population.at.arrival(smat, tab),
                    company = fun(smat, par)));
}

# todo: include author fixed effect
intensive.frame <- function(ef = extensive.frame(tab, twr, cosmat,
                                            neighbors.around, 0.5),
                            rdf = reviewer.dummy.frame(twr)) {
  ef2 <- ef[ef$approval>0,];
  return(cbind(ef2, rdf[rownames(ef2),]));
}


# only include reviewers with at least n reviews 
reviewer.dummy.frame <- function(twr = tweet.regard(),
                                 cutoff = slate.pubs$date[3],
                                 freq = 10,
                                 labels = rownames(tab)) {
  twrsub <- twr[twr$date < cutoff&twr$score>1,];
  reviewers <- names(which(summary(twrsub$author, Inf)>freq));

  rdf <- sapply(reviewers,
                function(x) {
                  sapply(split(twrsub$author, twrsub$src),
                         function(v) x %in% v);
                });
  rownames(rdf) <- labels[as.numeric(rownames(rdf))];

  return(rdf);
}

author.dummy.frame <- function(subtab =
                               tab[tab$date < slate.pubs$date[3] &
                                   !(is.retweet(tab$tweet)|
                                     has.url(tab$tweet)),],
                               freq = 5) {
  freq.authors <- names(which(summary(subtab$author, Inf) > freq));

  adf <- sapply(freq.authors,
                function(x) {
                  return(subtab$author == x);
                });

  return(adf);
}

# how many items in neighborhood at time of publication?
neighbors.at.arrival <- function(mat = cosmat,
                                 tab = read.all(),
                                 horizon = 0.5) {
  return(sapply(rownames(mat),
                function(x) {
                  smat <- submat(mat, rownames(tab)[tab$date <=
                                                    tab$date[rownames(tab)==x]]);
                  if(nrow(smat) > 0) {
                    return(sum(smat[x,] <= horizon)-1);
                  } else {
                    return(0);
                  }
                }));
}

population.at.arrival <- function(mat, tab) {
  sel <- rownames(mat);
  return(sapply(sel,
                function(x) {
                  length(intersect(sel,
                                   rownames(tab)[tab$date <
                                                 tab$date[rownames(tab)==x]]));
                }));
}



hurdle.frame <- function(tab, mat, twr, freq, par,
                         fun = neighbors.around,
                         par2 = par,
                         freq2 = freq,
                         cutoff = slate.pubs$date[3]) {
#  tab <- twit.all <- read.all()
#  corp <- table2Corpus(tab[!(is.retweet(tab$tweet)|has.url(tab$tweet)),])
#  cosmat <- dis.matrix(corp, "cosine")
#  twr <- tweet.regard();
#subtab =
#                               tab[tab$date < slate.pubs$date[3] &
#                                   !(is.retweet(tab$tweet)|
#                                     has.url(tab$tweet)),]
#smat <- submat(cosmat,rownames(subtab))
  id <- rownames(mat);
  pa <- peer.approval(twr);
  v <- numeric(length(id));
  names(v) <- id;
  pasel <- intersect(names(pa),id);
  v[pasel] <- pa[pasel];

  rdf <- reviewer.dummy.frame(twr, freq=freq2);
  rdf.all <- matrix(FALSE, length(id), ncol(rdf));
  rownames(rdf.all) <- id;
  colnames(rdf.all) <- colnames(rdf);
  rdf.all[intersect(rownames(rdf), id),] <- rdf[intersect(rownames(rdf), id),];
  
  return(data.frame(v = v,
                    age = cutoff - tab$date,
                    pna = neighbors.at.arrival(mat, tab)/population.at.arrival(mat, tab),
                    company = fun(mat, par2),
                    adf = author.dummy.frame(tab, freq),
                    rdf = rdf.all));
}
