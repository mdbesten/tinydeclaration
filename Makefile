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

# winners: http://www.slate.com/id/2258811/

PAGES = 12
RPP = 100
FID = 17366280765
UNTIL = 2010-07-05

.PHONY: all first.id

all: TinyDeclaration.json

%.json:
	for i in $$(seq ${PAGES}); do\
		curl ${PROXY} "http://search.twitter.com/search.json?q=%23$(basename $@)&rpp=${RPP}&page=$$i";\
	done > $@;

#  1081 ,"to_user_id"
#   1081 ,"text"
#   1081 ,"source"
#   1081 ,"metadata"
#   1081 ,"iso_language_code"
#   1081 ,"id"
#   1081 ,"geo"
#   1081 ,"from_user_id"
#   1081 ,"from_user"
#   1081 ,"created_at"
#  89 ,"to_user"

%.csv: %.json
	awk 'BEGIN{\
		RS = ",\"";\
		FS = "\":";\
		OFS = ",";\
		print "author, day, timestamp, id, title";\
	} /^created_at\"/{\
		submissionDate = $$2;\
		gsub("\"","", submissionDate);\
	} /^from_user\"/{\
		author = $$2;\
		gsub("\"","", author);\
	} /^from_user_id\"/{\
		uid = $$2;\
	} /^text\"/{\
		title = $$2;\
		gsub(",", "COMMA", title);\
		gsub("\"","", title);\
	} /^source\"/{\
		print author, submissionDate, uid, title;\
	}' $< > $@;

first.id: TinyDeclaration.json
	awk 'BEGIN{\
		RS = ",\"";\
		FS = "\":";\
		fid = 9^99;\
	} /^id\"/ {\
		id = $$2;\
		if(id < fid) {\
			fid = id;\
		}\
	} END {\
		print fid;\
	}' $<

