### IMPORT ###   
from html.parser import HTMLParser
import re
import sys
### CLASSES ###   
class PDALit2js(HTMLParser):
	def __init__(self,o,keep=None):
		if keep: keep=keep.replace(
				'letture','lettura1,responsorio1,lettura2,responsorio2,orazione').replace(
				'memoria','inno,lettura,lettura2,responsorio,responsorio2,ant3a,ant6a,ant9a,ant_ben,ant_magn,invocazioni,intercessioni,orazione,').replace(
				'domenica','ant_ben,ant_magn,orazione')+',title'
		HTMLParser.__init__(self)
		self.b=0
		self.i=0
		self.d=''
		self.keep = keep.split(',') if keep else None
		self.h=set()
		self.o=o
		self.s=''
		self.sal=0
		self.resp=''
	def flush(self,s=None):
		if self.s and self.keep:
			if not self.s in self.keep: self.s=None
		if self.s and not self.s in self.h:
			self.h.add(self.s)
			if self.s=='title':
				self.d=re.sub(r'[ /]*V\. *O Dio.*$','',self.d)
			self.d=re.sub(r'<b>[\s]*$','',self.d)
			self.d=re.sub(r'\s*\s*','',self.d)
			self.d=self.d.strip(' ')
			self.d=re.sub(r'</i>','</i> // ',self.d)
			self.d=re.sub(r'+',' // ',self.d)
			self.d=re.sub(r'',' / ',self.d)
			self.d=re.sub(r'([\"])',r'\\\1',self.d)
			print(",\n'",self.s,"':\"<b>",self.d,'"',sep='',end='',file=self.o)
		self.d=''
		self.s=s
	def handle_charref(self, ref):
		self.write("&#%s;"%ref)
	def handle_data(self, data):
		data=re.sub(r'\s+',' ',data)
		if self.b:
			if 0: pass
			elif 'Ant' in data:
				if '1^' in data:
					self.flush('ant1')
				elif '2^' in data:
					self.flush('ant2')
				elif '3^' in data:
					self.flush('ant3')
				elif 'TERZA' in data:
					self.flush('ant3a')
				elif 'SESTA' in data:
					self.flush('ant6a')
				elif 'NONA' in data:
					self.flush('ant9a')
				elif 'Ben' in data:
					self.flush('ant_ben')
				elif 'Magn' in data:
					self.flush('ant_magn')
				elif 'Ingresso' in data:
					self.flush('ant_ingr')
				elif 'Comunione' in data:
					self.flush('ant_comun')
				else:
					if self.s.startswith('sal'):
						self.flush('ant'+str(self.sal))
					elif self.s=='nunc_dimittis':
						self.flush('ant_nunc')
					else: self.flush('antifona')
			elif 'lettura' in data.lower():
				if 'Prima' in data:
					self.flush('lettura1')
					self.resp='1'
				elif 'Seconda' in data:
					self.flush('lettura2')
					self.resp='2'
				elif 'TERZA' in data:
					self.flush('lettura3a')
					self.resp=''
				elif 'SESTA' in data:
					self.flush('lettura6a')
					self.resp=''
				elif 'NONA' in data:
					self.flush('lettura9a')
					self.resp=''
				else:
					self.flush('lettura')
					self.resp=''
			elif 'inno' in data.lower():
				if 'TERZA' in data:
					self.flush('inno3a')
				elif 'SESTA' in data:
					self.flush('inno6a')
				elif 'NONA' in data:
					self.flush('inno9a')
				else:
					self.flush('inno')
			elif 'Colletta' in data:
				self.flush('colletta')
			elif 'Canto al Vangelo' in data:
				self.flush('vangelo')
			elif 'Sulle Offerte' in data:
				self.flush('offerte')
			elif 'Dopo la Comunione' in data:
				self.flush('comunione')
			elif re.search(r'CANTICO +DELLA',data):
				self.flush('magnificat')
			elif re.search(r'CANTICO\s+DI +ZACCARIA',data):
				self.flush('benedictus')
			elif re.search(r'CANTICO +di +SIMEONE',data):
				self.flush('nunc_dimittis')
			elif re.search(r'SALMO|CANTICO',data):
				self.sal+=1
				self.flush('sal'+str(self.sal))
			elif re.search(r'TE +DEUM',data):
				self.flush('tedeum')
			elif 'responsorio' in data.lower():
				self.flush('responsorio'+self.resp)
			elif 'Intercessioni' in data:
				self.flush('intercessioni')
			elif 'Invocazioni' in data:
				self.flush('invocazioni')
			elif 'Padre Nostro' in data and (self.s in ('invocazioni','intercessioni')):
				self.flush('padre_nostro')
			elif 'orazione' in data.lower():
				if self.s == 'lettura3a':
					self.flush('orazione3a')
				elif self.s == 'lettura6a':
					self.flush('orazione6a')
				elif (self.s == 'lettura9a'
					and 'lettura3a' in self.h):
					self.flush('orazione9a')
				else:
					self.flush('orazione')
			elif 'Versetto' in data:
				self.flush('versetto')
			elif 'INVITATORIO' in data:
				self.sal-=1
				self.flush('invitatorio')
			elif 'ESAME' in data:
				self.flush('esame')
		self.write(data)
	def handle_endtag(self, tag):
		if tag == 'b':
			self.write('</b>')
			self.b=0
		elif tag == 'body':
			self.flush()
		elif tag == 'i':
			self.write('</i>')
			self.i=0
	def handle_entityref(self, ref):
		if ref == 'nbsp': s=' '
		else: s="&%s;"%ref
		self.write(s)
	def handle_starttag(self, tag, attrs):
		if tag == 'b':
			self.write('<b>')
			self.b=1
		elif tag == 'br':
			self.write('')
		elif tag == 'i':
			self.write('<i>')
			self.i=1
		elif tag == 'p':
			self.write('')
	def write(self,data=None):
		self.d+=data
### MAIN ### 
keep=None
p=99
for i in range(1,len(sys.argv)):
	a=sys.argv[i]
	i=a.find('=')
	if i<0:
		o=re.sub(r'\.[^.]*$',r'.js',a)
		print(a,'->',o,file=sys.stderr)
		a=open(a,encoding='cp1252')
		o=open(o,'w')
		w=PDALit2js(o,keep)
		print("STACK.pop()({",
					"prior:",p,
					sep='',end='',file=o)
		l=a.read()
		l=l.replace('&nbsp;',' ').replace('&amp;','e')
		w.feed(l)
		print('\n})',file=o)
		a.close()
		o.close()
		continue
	k=a[0:i]
	a=a[i+1:]
	if k.startswith('k'): keep=a
	elif k.startswith('t'): t=a
	elif k.startswith('p'): p=a 
# vim:set sw=2 ts=2 sts=2:
