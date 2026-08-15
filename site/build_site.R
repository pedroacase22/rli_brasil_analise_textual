options(stringsAsFactors = FALSE, encoding = "UTF-8")
`%||%` <- function(x,y) if(is.null(x)) y else x

args_all <- commandArgs(trailingOnly=FALSE); file_arg <- grep("^--file=",args_all,value=TRUE)
script_path <- if(length(file_arg)) normalizePath(sub("^--file=","",file_arg[1])) else file.path(normalizePath(getwd()),"build_site.R")
script_dir <- dirname(script_path)
packaged <- file.exists(file.path(script_dir,"leis_inovacao_textos_integrais.zip"))
if (packaged) {
  root <- script_dir; out_dir <- script_dir
  source_dir <- file.path(tempdir(),"rli_corpus"); dir.create(source_dir,recursive=TRUE,showWarnings=FALSE)
  unzip(file.path(script_dir,"leis_inovacao_textos_integrais.zip"),exdir=source_dir)
} else {
  root <- normalizePath(file.path(script_dir,"..",".."))
  source_dir <- file.path(root, "outputs", "leis_txt")
  out_dir <- file.path(root, "outputs", "rli_nuvens_palavras")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

index <- read.csv(file.path(source_dir, "indice.csv"), fileEncoding = "UTF-8-BOM", check.names = FALSE)

regions <- c(
  AC="Norte", AP="Norte", AM="Norte", PA="Norte", RO="Norte", RR="Norte", TO="Norte",
  AL="Nordeste", BA="Nordeste", CE="Nordeste", MA="Nordeste", PB="Nordeste", PE="Nordeste",
  PI="Nordeste", RN="Nordeste", SE="Nordeste", DF="Centro-Oeste", GO="Centro-Oeste",
  MT="Centro-Oeste", MS="Centro-Oeste", ES="Sudeste", MG="Sudeste", RJ="Sudeste", SP="Sudeste",
  PR="Sul", RS="Sul", SC="Sul"
)
index$regiao <- unname(regions[index$uf])
index$regiao[is.na(index$regiao)] <- "Brasil"
index$ano <- sub(".*?/", "", index$lei_numero)
index$ano[!grepl("^[0-9]{4}$", index$ano)] <- ""

stop_pt <- c(
  "a","à","ao","aos","aquela","aquelas","aquele","aqueles","aquilo","as","às","até","com","como","da","das","de","dela","delas","dele","deles","depois","deste","do","dos","e","é","ela","elas","ele","eles","em","entre","era","eram","essa","essas","esse","esses","esta","está","estão","estas","este","estes","eu","foi","for","foram","há","isso","isto","já","lhe","lhes","mais","mas","me","mesmo","meu","minha","muito","na","não","nas","nem","no","nos","nós","nossa","nosso","num","numa","o","os","ou","para","pela","pelas","pelo","pelos","por","poder","dever","qual","quando","que","quem","se","sem","ser","seu","seus","só","sob","sobre","sua","suas","também","tem","tendo","ter","todo","todos","um","uma","umas","uns","vai","vão",
  "art","artigo","artigos","caput","capítulo","capitulo","seção","secao","inciso","incisos","alínea","alinea","parágrafo","paragrafo","único","unico","lei","leis","decreto","complementar","ordinária","ordinaria","nº","nr","n°","disposto","disposições","disposicoes","forma","termos","efeitos","fins","fica","ficam","poderá","poderão","deverá","deverão","respectivo","respectivos","respectiva","respectivas","previsto","previstos","prevista","previstas","mediante","acordo","data","publicação","publicacao","vigor","revogadas","redação","redacao","dada","alterado","alterada","seguinte","seguintes","âmbito","municipal","estadual","federal","estado","município","municipio","governador","prefeito","executivo","legislativo","administração","administracao","pública","publica","público","publico",
  "página","pagina","início","inicio","voltar","pesquisa","livre","temática","tematica","clique","aqui","acessar","íntegra","integra","compartilhar","fechar","cookies","privacidade","portal","menu","imprimir","email","fonte","tamanho","aguarde","download","documento","documentos","arquivo","arquivos","legislação","legislacao","oficial","diário","diario","DOE","DOM","publicado","publicada","copyright","site","busca","home","navegação","navegacao",
  "janeiro","fevereiro","março","marco","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro"
)
stop_norm <- unique(tolower(iconv(stop_pt, to="ASCII//TRANSLIT")))

# Dicionário editável pelo usuário. "exato" altera apenas a forma indicada;
# "prefixo" também corrige suas flexões; "excluir" remove ruído de captura.
dict_path <- file.path(out_dir, "dicionario_termos.csv")
base_dictionary <- data.frame(
  padrao=c("proj","projet","execuc","gest","fundac","itulo","ublica","ublicas","ublico","ublicos"),
  substituicao=c("projeto","projeto","execucao","gestao","fundacao","","","","",""),
  tipo=c("exato","prefixo","prefixo","exato","prefixo","excluir","excluir","excluir","excluir","excluir"),
  stringsAsFactors=FALSE
)
dictionary <- base_dictionary
if (file.exists(dict_path)) {
  custom_dictionary <- tryCatch(read.csv(dict_path, stringsAsFactors=FALSE), error=function(e) NULL)
  if (!is.null(custom_dictionary) && all(c("padrao","substituicao","tipo") %in% names(custom_dictionary))) {
    custom_dictionary <- custom_dictionary[!custom_dictionary$padrao %in% base_dictionary$padrao,]
    dictionary <- rbind(base_dictionary, custom_dictionary)
  }
}

normalize_tokens <- function(text) {
  text <- gsub("\uFFFD", "", text, fixed=TRUE)
  text <- gsub("<[^>]+>", " ", text)
  html_entities <- c(amp="&",quot='"',apos="'",nbsp=" ",aacute="á",agrave="à",acirc="â",atilde="ã",auml="ä",eacute="é",egrave="è",ecirc="ê",euml="ë",iacute="í",igrave="ì",icirc="î",iuml="ï",oacute="ó",ograve="ò",ocirc="ô",otilde="õ",ouml="ö",uacute="ú",ugrave="ù",ucirc="û",uuml="ü",ccedil="ç",Aacute="Á",Agrave="À",Acirc="Â",Atilde="Ã",Eacute="É",Ecirc="Ê",Iacute="Í",Oacute="Ó",Ocirc="Ô",Otilde="Õ",Uacute="Ú",Ccedil="Ç")
  for (entity in names(html_entities)) text <- gsub(paste0("&",entity,";"),html_entities[[entity]],text,fixed=TRUE)
  text <- gsub("&#160;|&#xA0;", " ", text, ignore.case=TRUE)
  text <- gsub("&[[:alnum:]#]+;", " ", text)
  text <- iconv(text, from="UTF-8", to="ASCII//TRANSLIT", sub=" ")
  text <- tolower(text)
  text <- gsub("https?://[^[:space:]]+|www\\.[^[:space:]]+", " ", text)
  text <- gsub("[^a-z]+", " ", text)
  tok <- unlist(strsplit(text, "[[:space:]]+"), use.names=FALSE)
  tok <- tok[nchar(tok) >= 4 & nchar(tok) <= 28]
  canonical <- c(inovac="inovacao", tecnol="tecnologia", cient="ciencia", pesquis="pesquisa", desenvolv="desenvolvimento", empreend="empreendedorismo", instituic="instituicao", propried="propriedade", universid="universidade", financ="financiamento", sustent="sustentabilidade", cooperac="cooperacao", administrac="administracao", transfer="transferencia")
  for (prefix in names(canonical)) tok[startsWith(tok,prefix)] <- canonical[[prefix]]
  for (i in seq_len(nrow(dictionary))) {
    hit <- if (dictionary$tipo[i] == "prefixo") startsWith(tok, dictionary$padrao[i]) else tok == dictionary$padrao[i]
    if (dictionary$tipo[i] == "excluir") tok <- tok[!hit] else tok[hit] <- dictionary$substituicao[i]
  }
  tok[grepl("^(p.?blica|ublica|publica)$",tok)] <- "publica"
  repairs <- c(inovao="inovacao",imulo="estimulo",ogico="tecnologico",ogicos="tecnologicos",ogica="tecnologica",ogicas="tecnologicas",omico="economico",omica="economica",omicos="economicos",ecnico="tecnico",ecnicos="tecnicos",ecnica="tecnica",ecnicas="tecnicas",jurdica="juridica",jurdicas="juridicas",idico="juridico",includo="incluido",includos="incluidos",provisria="provisoria",provisrias="provisorias",ocios="negocios",comit="comite",ipios="municipios",constitu="constitui",enio="convenio",enios="convenios",servios="servicos",oprio="proprio",opria="propria",ucleo="nucleo",otese="hipotese",contribuio="contribuicao",pargrafo="paragrafo",administrao="administracao",egicas="estrategicas",preju="prejuizo",pblico="publico",societ="sociedade")
  tok[tok %in% names(repairs)] <- unname(repairs[tok[tok %in% names(repairs)]])
  tok <- tok[!tok %in% stop_norm]
  # Algarismos romanos isolados representam incisos/subdivisões legais.
  tok <- tok[!grepl("^(?=[ivxlcdm]+$)[ivxlcdm]+$", tok, perl=TRUE)]
  tok <- tok[!grepl("^(http|www|html|asp|pdf|mailto|javascript|identificador|documento|nbsp|avel|veis|cao|coes|munic|encia|ncia|ogica|ogico|criac|ublico|econ|desta|trata)$", tok)]
  # Auditoria conservadora: resíduos de OCR/HTML e cabeçalhos jurídicos que
  # não constituem conceitos substantivos são excluídos, sem imputação.
  lexical_noise <- c("participac","ifico","ifica","aria","espec","itica","agrafo","encias","ipio","ario","arios","vigncia","explorac","aplicac","legislac","realizac","orio","promoc","soluc","contratac","icio","icios","conv","concess","informac","estrat","constituic","prestac","protec","organizac","crit","ificos","ificas","gerac","produo","arias","remunerac","capacitac","extens","regulat","idica","orios","redao","condic","iveis","iticas","disposic","utilizac","necess","erios","compet","ivel","orcament","ancia","aveis","ipios","respons","secret","sero","erio","laborat","nico","ater","tempor","institu")
  tok <- tok[!tok %in% lexical_noise]
  tok <- tok[!grepl("[bcdfgjkpqvw]$",tok) | tok %in% c("startup")]
  tok
}

texts <- lapply(file.path(source_dir, "txt", index$arquivo_txt), function(p) paste(readLines(p, warn=FALSE, encoding="UTF-8"), collapse=" "))
tokens <- lapply(texts, normalize_tokens)

# Unidades normativas para coocorrência: artigo é a única especificação.
segment_units <- function(text, unit="article") {
  x <- gsub("\uFFFD", "", text, fixed=TRUE)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&nbsp;|&#160;|&#xA0;", " ", x, ignore.case=TRUE)
  x <- gsub("[\r\n]+", " ", x)
  pattern <- "(?i)(?=\\bart\\.?\\s*[0-9]+[ºo°]?)"
  u <- unlist(strsplit(x, pattern, perl=TRUE), use.names=FALSE)
  u <- trimws(u[nchar(trimws(u)) >= 30])
  u
}

network_data <- function(ids, unit="article", max_nodes=26, max_edges=70) {
  units <- unlist(lapply(texts[ids], segment_units, unit=unit), use.names=FALSE)
  ut <- lapply(units, function(x) unique(normalize_tokens(x)))
  ut <- ut[lengths(ut)>=2]
  if (!length(ut)) return(list(nodes=list(),edges=list(),metrics=list(units=0,density=0,mean_degree=0,communities=0)))
  df <- sort(table(unlist(ut)),decreasing=TRUE); vocab <- names(head(df[df>=2],max_nodes))
  if (length(vocab)<2) return(list(nodes=list(),edges=list(),metrics=list(units=length(ut),density=0,mean_degree=0,communities=0)))
  M <- matrix(0,length(vocab),length(vocab),dimnames=list(vocab,vocab))
  for (z in ut) { z<-intersect(z,vocab); if(length(z)>1) M[z,z]<-M[z,z]+1 }
  diagv <- diag(M); E <- which(upper.tri(M)&M>0,arr.ind=TRUE)
  assoc <- M[E]/sqrt(diagv[E[,1]]*diagv[E[,2]])
  ed <- data.frame(a=E[,1],b=E[,2],count=M[E],association=assoc)
  ed <- ed[order(-ed$association,-ed$count),]; ed <- head(ed[ed$association>=.12,],max_edges)
  if (!nrow(ed)) return(list(nodes=list(),edges=list(),metrics=list(units=length(ut),density=0,mean_degree=0,communities=0)))
  keep <- sort(unique(c(ed$a,ed$b))); vocab<-vocab[keep]; oldnew<-setNames(seq_along(keep),keep); ed$a<-oldnew[as.character(ed$a)];ed$b<-oldnew[as.character(ed$b)]
  n<-length(vocab); A<-matrix(0,n,n); for(i in seq_len(nrow(ed))){A[ed$a[i],ed$b[i]]<-A[ed$b[i],ed$a[i]]<-ed$association[i]}
  degree<-rowSums(A>0); strength<-rowSums(A)
  # Comunidades por clustering espectral; k é escolhido pelo maior eigengap
  # do Laplaciano normalizado (entre 2 e 6 grupos), com semente fixa.
  if(n>=4){dw<-rowSums(A);S<-diag(1/sqrt(pmax(dw,1e-9)));L<-diag(n)-S%*%A%*%S;ev<-eigen(L,symmetric=TRUE);ord<-order(ev$values);vals<-ev$values[ord];vec<-ev$vectors[,ord,drop=FALSE];kmax<-min(6,n-1);ks<-2:kmax;gaps<-vapply(ks,function(k)vals[k+1]-vals[k],numeric(1));k<-ks[which.max(gaps)];set.seed(2026);lab<-kmeans(vec[,seq_len(k),drop=FALSE],centers=k,nstart=30)$cluster}else lab<-rep(1,n)
  # Distâncias geodésicas ponderadas e centralidade de intermediação.
  D<-matrix(Inf,n,n);diag(D)<-0;D[A>0]<-1/A[A>0];for(k in seq_len(n))D<-pmin(D,outer(D[,k],D[k,],"+"));bet<-numeric(n)
  for(s in seq_len(n))for(t in seq_len(n))if(s<t&&is.finite(D[s,t]))for(v in setdiff(seq_len(n),c(s,t)))if(abs(D[s,v]+D[v,t]-D[s,t])<1e-7)bet[v]<-bet[v]+1
  DD<-D;DD[!is.finite(DD)]<-max(DD[is.finite(DD)])*1.2;xy<-tryCatch(cmdscale(DD,k=2),error=function(e)cbind(cos(seq(0,2*pi,length.out=n+1)[-1]),sin(seq(0,2*pi,length.out=n+1)[-1])))
  scale01<-function(x) if(diff(range(x))==0) rep(.5,length(x)) else (x-min(x))/diff(range(x)); x<-8+84*scale01(xy[,1]);y<-8+84*scale01(xy[,2])
  nodes<-lapply(seq_len(n),function(i)list(id=i,label=vocab[i],frequency=unname(df[vocab[i]]),degree=degree[i],strength=round(strength[i],3),betweenness=bet[i],community=lab[i],x=round(x[i],2),y=round(y[i],2)))
  edges<-lapply(seq_len(nrow(ed)),function(i)list(source=ed$a[i],target=ed$b[i],count=ed$count[i],association=round(ed$association[i],3)))
  list(nodes=nodes,edges=edges,metrics=list(units=length(ut),density=round(2*nrow(ed)/(n*(n-1)),3),mean_degree=round(mean(degree),2),communities=length(unique(lab))))
}

freq_table <- function(ids, max_terms=140) {
  x <- unlist(tokens[ids], use.names=FALSE)
  tab <- sort(table(x), decreasing=TRUE)
  tab <- tab[seq_len(min(length(tab), max_terms))]
  data.frame(word=names(tab), freq=as.integer(tab), stringsAsFactors=FALSE)
}

layout_cloud <- function(df, seed=1, width=780, height=650) {
  set.seed(seed)
  if (!nrow(df)) return(list())
  df <- head(df, 110)
  lo <- log1p(df$freq)
  sizes <- 10 + 35 * (lo-min(lo))/max(1e-9, max(lo)-min(lo))
  placed <- list(); output <- vector("list", nrow(df))
  colors <- c("#152018", "#2D7A48", "#50B96A", "#326B46", "#66706A", "#8BAA94")
  for (i in seq_len(nrow(df))) {
    fs <- sizes[i]; rot <- if (i > 12 && runif(1)<.16) 90 else 0
    # Caixa próxima da largura visual da fonte: compacta, mas ainda segura.
    tw <- max(24, nchar(df$word[i]) * fs * .58); th <- fs
    if (rot==90) { z<-tw; tw<-th; th<-z }
    ok <- FALSE
    for (attempt in 1:1800) {
      angle <- attempt * 0.49; radius <- 7.2 * sqrt(attempt)
      cx <- width/2 + cos(angle)*radius + rnorm(1,0,2)
      cy <- height/2 + sin(angle)*radius*.62 + rnorm(1,0,1.5)
      rect <- c(cx-tw/2-2.5, cy-th/2-2, cx+tw/2+2.5, cy+th/2+2)
      inside <- rect[1] > 8 && rect[2] > 8 && rect[3] < width-8 && rect[4] < height-8
      overlap <- any(vapply(placed, function(p) !(rect[3]<p[1] || rect[1]>p[3] || rect[4]<p[2] || rect[2]>p[4]), logical(1)))
      if (inside && !overlap) { ok<-TRUE; placed[[length(placed)+1]]<-rect; break }
    }
    if (!ok) next
    output[[i]] <- list(word=df$word[i], freq=df$freq[i], x=round(cx/width*100,2), y=round(cy/height*100,2), size=round(fs,1), rotate=rot, color=colors[1+(i-1)%%length(colors)])
  }
  Filter(Negate(is.null), output)
}

clouds <- list(); networks <- list(); add_cloud <- function(id, title, subtitle, ids, type, seed) {
  f <- freq_table(ids)
  clouds[[id]] <<- list(id=id,title=title,subtitle=subtitle,type=type,count=length(ids),terms=sum(f$freq),words=layout_cloud(f,seed))
  networks[[id]] <<- list(article=network_data(ids,"article"))
}

all_ids <- seq_len(nrow(index)); federal <- which(index$scope=="federal"); state <- which(index$scope=="estadual"); muni <- which(index$scope=="municipal")
add_cloud("agg_nacional", "Nuvem agregada nacional", "Todas as normas federais, estaduais e municipais do repositório.", all_ids, "Nacional", 100)
add_cloud("agg_federal", "Leis nacionais (federais)", "Vocabulário agregado das leis federais.", federal, "Federal", 101)
add_cloud("agg_estadual", "Todas as leis estaduais", "Vocabulário agregado das leis estaduais.", state, "Estadual", 102)
add_cloud("agg_municipal", "Todas as leis municipais", "Vocabulário agregado das leis e decretos municipais.", muni, "Municipal", 103)
for (reg in c("Norte","Nordeste","Centro-Oeste","Sudeste","Sul")) {
  ids <- which(index$scope=="estadual" & index$regiao==reg)
  add_cloud(paste0("reg_",tolower(gsub("-","_",reg))), paste("Região",reg), "Agregação das leis estaduais da região.", ids, "Região", 200+match(reg,c("Norte","Nordeste","Centro-Oeste","Sudeste","Sul")))
}
for (i in seq_len(nrow(index))) {
  loc <- if (index$scope[i]=="federal") "Brasil" else if (index$scope[i]=="estadual") index$uf[i] else paste(index$municipio[i], index$uf[i], sep=" · ")
  id <- paste0("lei_",sprintf("%02d",i))
  f <- freq_table(i)
  clouds[[id]] <- list(id=id,title=index$lei_numero[i],subtitle=paste(index$lei_nome[i],loc,sep=" · "),type=index$scope[i],count=1,terms=sum(f$freq),words=layout_cloud(f,500+i))
  networks[[id]] <- list(article=network_data(i,"article"))
}

law_meta <- lapply(seq_len(nrow(index)), function(i) list(
  id=paste0("lei_",sprintf("%02d",i)), numero=index$lei_numero[i], nome=index$lei_nome[i], scope=index$scope[i], uf=index$uf[i], municipio=index$municipio[i], regiao=index$regiao[i], ano=index$ano[i], fonte=index$fonte[i], url=index$url_integra[i], arquivo=index$arquivo_txt[i]
))
# Exportações PNG são produzidas pelo próprio R, uma por recorte.
png_dir<-file.path(out_dir,"png");dir.create(png_dir,recursive=TRUE,showWarnings=FALSE)
for(id in names(clouds)){w<-clouds[[id]]$words;png(file.path(png_dir,paste0(id,".png")),width=1560,height=1300,res=150,bg="white");par(mar=c(0,0,0,0));plot.new();plot.window(c(0,1),c(0,1));if(length(w))for(z in w)text(z$x/100,1-z$y/100,z$word,cex=z$size/17,col=z$color,font=2,srt=z$rotate);dev.off()}
data <- list(clouds=clouds,networks=networks,laws=law_meta,stats=list(laws=nrow(index),states=length(unique(index$uf[index$uf!=""])),municipalities=length(unique(index$municipio[index$municipio!=""]))))
json <- jsonlite::toJSON(data, auto_unbox=TRUE, pretty=FALSE, na="null")
writeLines(paste0("window.RLI_DATA=",json,";"), file.path(out_dir,"data.js"), useBytes=TRUE)

css <- r"---(
body .topbar .brand{width:auto;height:42px;overflow:visible}body .topbar .brand-logo{width:42px;height:42px;transform:scale(1.45)}
html body{--green:#00BF63;--green-dark:#008d49;--ink:#111827;--muted:#6B7280;--line:#E5E7EB;font-family:"Source Sans 3","Source Sans",sans-serif;font-size:16px;font-weight:400}body .container{width:min(1200px,calc(100% - 40px))}body .topbar{height:auto}body .nav{min-height:58px;padding:14px 0}body .brand{width:auto;height:auto;overflow:visible;gap:14px;font-size:15px;font-weight:600}body .brand-logo{width:42px;height:42px;transform:scale(1.45);object-position:center}body .brand-title{font-size:15px;font-weight:600;line-height:1.2;white-space:nowrap}body .links{gap:28px}body .links a{font-size:14px;font-weight:500;color:var(--muted)}body .hero{min-height:0;padding:86px 0 64px;text-align:center}body .hero-inner{max-width:760px}body .hero h1{font-size:clamp(38px,5vw,48px);line-height:1.08;letter-spacing:-.035em;margin:0 0 18px}body .hero p{font-size:18px;line-height:1.5;max-width:760px;margin-inline:auto}body .actions{justify-content:center;margin-top:24px;gap:12px}body .btn{padding:11px 18px;border-radius:4px;font-weight:500}body .about,body .explore,body .network-section,body .downloads{padding:34px 0}body .eyebrow{font-size:12px;letter-spacing:.12em;font-weight:700;margin-bottom:6px}body .section-title{font-size:32px;font-weight:650;letter-spacing:0;margin:0 0 10px}body .lead{font-size:16px;line-height:1.65;max-width:920px}body .cards{gap:16px;margin-top:24px}body .card{padding:16px;min-height:0;border-radius:4px}body .card h3{font-size:16px;font-weight:700;margin:0 0 6px}body .card p{font-size:16px;line-height:1.55;margin:0}body .explorer{margin-top:24px;min-height:0;border-radius:4px;overflow:hidden}body .sidebar{padding:20px}body .sidebar h3{font-size:18px;font-weight:650;margin:0 0 8px}body .field{margin-top:18px}body .field label{font-size:12px;letter-spacing:.08em;font-weight:600}body input,body select{padding:11px 12px;border-radius:4px;color:var(--muted)}body .stage{padding:24px}body .stage-head h3{font-size:24px;font-weight:650}body .filter-help,body .filter-note{font-size:14px}body .network-cards{margin-top:24px}body .method-note{font-size:16px;line-height:1.65}body .result-label{margin-top:34px;padding-top:22px;font-size:12px;letter-spacing:.12em}body .network-explorer{margin-top:18px}body .network-stats div{padding:14px 16px}body .network-stats b{font-size:22px}body .network-canvas{height:610px}body .download-grid{margin-top:24px;gap:16px}body .download-card{padding:16px;border-radius:4px}body .download-card b{font-size:16px}body .return-rli{margin-top:34px;padding:24px;border-radius:4px}body .footer{padding:18px 0;font-size:13px}
.topbar .brand-logo{object-position:center;transform:scale(1.5)}.topbar .brand{width:190px;height:72px;overflow:hidden;display:flex;align-items:center;justify-content:center}
a.download-action{display:inline-flex;align-items:center;margin-top:28px}.stage .details{min-height:42px;align-items:flex-start;flex-wrap:wrap}.brand-logo{width:190px;height:72px;display:block;object-fit:contain;object-position:left center}.brand{text-decoration:none}.network-cards{margin-top:38px}.result-label{margin-top:54px;padding-top:28px;border-top:1px solid var(--line);font-size:13px;letter-spacing:.16em;text-transform:uppercase;color:var(--green-dark);font-weight:900}.downloads .download-grid{grid-template-columns:repeat(2,1fr)}.return-rli{margin-top:52px;padding:34px;border:1px solid var(--line);background:var(--soft);display:flex;align-items:center;justify-content:space-between;gap:28px}.return-rli p{margin:0;color:var(--muted);font-size:17px;line-height:1.55;max-width:720px}.return-rli .btn{white-space:nowrap}@media(max-width:850px){.brand-logo{width:145px;height:62px}.downloads .download-grid{grid-template-columns:1fr}.return-rli{align-items:flex-start;flex-direction:column}}
:root{--green:#52b96b;--green-dark:#277443;--ink:#182018;--muted:#66706a;--line:#dbe1dc;--soft:#f5f7f5;--white:#fff}*{box-sizing:border-box}html{scroll-behavior:smooth}body{margin:0;font-family:Inter,ui-sans-serif,system-ui,-apple-system,"Segoe UI",sans-serif;color:var(--ink);background:#fff}a{color:inherit}.container{width:min(1180px,calc(100% - 40px));margin:auto}.topbar{height:98px;border-bottom:1px solid var(--line);display:flex;align-items:center;background:#fff;position:sticky;top:0;z-index:20}.nav{display:flex;align-items:center;justify-content:space-between}.brand{display:flex;gap:18px;align-items:center;font-size:22px;font-weight:800}.mark{font-size:30px;color:var(--green);letter-spacing:-2px}.links{display:flex;gap:30px}.links a{text-decoration:none;font-weight:700;color:#465048}.hero{min-height:590px;display:flex;align-items:center}.hero-inner{max-width:900px}.eyebrow{font-size:14px;letter-spacing:.26em;text-transform:uppercase;color:var(--green);font-weight:900;margin-bottom:24px}.hero h1{font-size:clamp(48px,7vw,86px);letter-spacing:-.055em;line-height:.96;margin:0 0 28px;max-width:950px}.hero p{font-size:23px;line-height:1.55;color:var(--muted);max-width:860px}.actions{display:flex;gap:14px;margin-top:34px}.btn{padding:15px 24px;border:1px solid var(--line);font-weight:800;text-decoration:none;background:#fff}.btn.primary{background:var(--green);color:#fff;border-color:var(--green)}.about{background:var(--soft);border-block:1px solid var(--line);padding:105px 0}.section-title{font-size:clamp(36px,4vw,56px);letter-spacing:-.04em;margin:0 0 24px}.lead{font-size:20px;line-height:1.65;color:var(--muted);max-width:1000px}.cards{display:grid;grid-template-columns:repeat(3,1fr);gap:18px;margin-top:52px}.card{background:#fff;border:1px solid var(--line);padding:30px;min-height:180px}.card h3{color:var(--green);font-size:23px}.card p{color:var(--muted);line-height:1.55}.explore{padding:110px 0}.explorer{display:grid;grid-template-columns:330px 1fr;border:1px solid var(--line);min-height:720px;margin-top:46px}.sidebar{padding:26px;border-right:1px solid var(--line);background:#fafbfa}.sidebar h3{margin:0 0 18px}.cut-buttons{display:grid;gap:8px}.cut{border:1px solid var(--line);background:#fff;text-align:left;padding:12px 13px;font-weight:750;cursor:pointer}.cut.active,.cut:hover{border-color:var(--green);color:var(--green-dark);background:#f1fbf3}.field{margin-top:22px}.field label{display:block;font-size:12px;text-transform:uppercase;letter-spacing:.1em;font-weight:850;margin-bottom:7px;color:var(--muted)}input,select{width:100%;padding:12px;border:1px solid var(--line);background:#fff;font:inherit}.clear{margin-top:12px;background:none;border:0;color:var(--green-dark);font-weight:800;cursor:pointer}.stats{display:grid;grid-template-columns:1fr 1fr;gap:8px;margin-top:24px}.stat{border-top:1px solid var(--line);padding-top:12px}.stat b{display:block;font-size:24px}.stat span{font-size:12px;color:var(--muted)}.law-list{margin-top:20px;max-height:245px;overflow:auto;display:grid;gap:6px}.law-item{border:0;background:transparent;text-align:left;padding:9px;cursor:pointer;font-size:13px;border-left:3px solid transparent}.law-item:hover,.law-item.active{background:#fff;border-left-color:var(--green)}.stage{padding:32px;min-width:0}.stage-head{display:flex;justify-content:space-between;gap:20px;align-items:start}.stage-head h3{font-size:28px;margin:0 0 7px}.stage-head p{margin:0;color:var(--muted)}.badge{border:1px solid var(--line);padding:7px 10px;font-size:12px;font-weight:800;white-space:nowrap}.cloud{height:570px;position:relative;overflow:hidden;margin-top:22px;background:radial-gradient(circle at 50% 50%,#fbfdfb,#fff 68%)}.word{position:absolute;transform:translate(-50%,-50%);font-weight:850;line-height:1;white-space:nowrap;cursor:pointer;transition:.16s ease}.word:hover{color:#111!important;transform:translate(-50%,-50%) scale(1.08);z-index:4}.word.rot{writing-mode:vertical-rl}.details{border-top:1px solid var(--line);padding-top:16px;display:flex;justify-content:space-between;gap:20px;color:var(--muted);font-size:14px}.details a{color:var(--green-dark);font-weight:800}.footer{border-top:1px solid var(--line);padding:44px 0;color:var(--muted)}.footer b{color:var(--ink)}@media(max-width:850px){.links{display:none}.hero{min-height:520px}.hero p{font-size:19px}.cards{grid-template-columns:1fr}.explorer{grid-template-columns:1fr}.sidebar{border-right:0;border-bottom:1px solid var(--line)}.cloud{height:480px}.container{width:min(100% - 26px,1180px)}}
.cloud{height:650px}
.filter-help,.filter-note{color:var(--muted);font-size:14px;line-height:1.55}.filter-help{margin:0 0 28px}.filter-note{margin-top:30px;padding-top:18px;border-top:1px solid var(--line)}#viewType,#viewChoice{min-height:48px;border-radius:0;cursor:pointer}#viewType:focus,#viewChoice:focus{outline:2px solid rgba(82,185,107,.25);border-color:var(--green)}
.download-action{margin-top:20px;cursor:pointer}.network-section{padding:110px 0;background:var(--soft);border-block:1px solid var(--line)}.network-toolbar{display:flex;justify-content:space-between;align-items:end;gap:30px;margin:42px 0 20px}.network-toolbar>div{width:330px}.network-toolbar label{display:block;font-size:12px;text-transform:uppercase;letter-spacing:.1em;font-weight:850;margin-bottom:7px;color:var(--muted)}.network-toolbar p{text-align:right;color:var(--muted)}.network-toolbar b{color:var(--ink)}.network-stats{display:grid;grid-template-columns:repeat(4,1fr);border:1px solid var(--line);background:#fff}.network-stats div{padding:18px 22px;border-right:1px solid var(--line)}.network-stats div:last-child{border:0}.network-stats b{font-size:25px;display:block}.network-stats span{font-size:12px;color:var(--muted)}.network-canvas{height:650px;background:#fff;border:1px solid var(--line);border-top:0}.network-canvas svg{width:100%;height:100%}.network-canvas text{font:700 12px Inter,Arial,sans-serif;fill:#243029;paint-order:stroke;stroke:#fff;stroke-width:3px;stroke-linejoin:round}.method-note{margin-top:22px;color:var(--muted);line-height:1.65}.method-note b{color:var(--ink)}.downloads{padding:100px 0}.download-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:16px;margin-top:42px}.download-card{display:flex;flex-direction:column;gap:10px;padding:28px;border:1px solid var(--line);text-decoration:none}.download-card:hover{border-color:var(--green);background:#f4fbf5}.download-card b{font-size:20px}.download-card span{color:var(--muted)}@media(max-width:850px){.network-toolbar{display:block}.network-toolbar>div{width:100%}.network-toolbar p{text-align:left}.network-stats{grid-template-columns:1fr 1fr}.download-grid{grid-template-columns:1fr}.network-canvas{height:520px}}
)---"
writeLines(css,file.path(out_dir,"styles.css"),useBytes=TRUE)

js <- r"---(
const D=window.RLI_DATA;let current="agg_nacional",networkCurrent="agg_nacional";const $=s=>document.querySelector(s);
function esc(s){return String(s??"").replace(/[&<>]/g,m=>({"&":"&amp;","<":"&lt;",">":"&gt;"}[m]))}
const choices={panorama:[["agg_nacional","Todas as leis"],["agg_federal","Leis nacionais (federais)"],["agg_estadual","Todas as leis estaduais"],["agg_municipal","Todas as leis municipais"]],regiao:[["reg_norte","Norte"],["reg_nordeste","Nordeste"],["reg_centro_oeste","Centro-Oeste"],["reg_sudeste","Sudeste"],["reg_sul","Sul"]],lei:D.laws.map(x=>[x.id,`${x.numero} — ${x.nome}${x.uf?` (${x.uf})`:''}`])};
function norm(s){return String(s).normalize('NFD').replace(/[\u0300-\u036f]/g,'').toLowerCase()}
function filteredItems(type,q){let items=choices[type];return type==="lei"&&q?items.filter(x=>norm(x[1]).includes(norm(q))):items}
function renderChoices(){let type=$("#viewType").value,items=filteredItems(type,$("#lawSearch").value);$("#viewChoice").innerHTML=items.length?items.map(x=>`<option value="${x[0]}">${esc(x[1])}</option>`).join(''):'<option value="">Nenhuma lei encontrada</option>';if(items.length)show(items[0][0])}
function populateChoices(){let type=$("#viewType").value;$("#lawSearchWrap").hidden=type!=="lei";if(type!=="lei")$("#lawSearch").value="";$("#choiceLabel").textContent=type==="lei"?"3. Escolha a lei":type==="regiao"?"2. Escolha a região (somente leis estaduais)":"2. Escolha a abrangência";renderChoices()}
function renderNetworkChoices(){let type=$("#networkViewType").value,items=filteredItems(type,$("#networkLawSearch").value);$("#networkViewChoice").innerHTML=items.length?items.map(x=>`<option value="${x[0]}">${esc(x[1])}</option>`).join(''):'<option value="">Nenhuma lei encontrada</option>';if(items.length)showNetwork(items[0][0])}
function populateNetworkChoices(){let type=$("#networkViewType").value;$("#networkLawSearchWrap").hidden=type!=="lei";if(type!=="lei")$("#networkLawSearch").value="";$("#networkChoiceLabel").textContent=type==="lei"?"3. Escolha a lei":type==="regiao"?"2. Escolha a região (somente leis estaduais)":"2. Escolha a abrangência";renderNetworkChoices()}
function show(id){current=id;let c=D.clouds[id];if(!c)return;$("#cloudTitle").textContent=c.title;$("#cloudSub").textContent=c.subtitle;$("#badge").textContent=c.type;$("#lawCount").textContent=c.count;$("#termCount").textContent=c.terms.toLocaleString("pt-BR");let scale=Math.min(1,$("#cloud").clientWidth/780);$("#cloud").innerHTML=c.words.map(w=>`<span class="word ${w.rotate?"rot":""}" title="${w.freq} ocorrências" style="left:${w.x}%;top:${w.y}%;font-size:${(w.size*scale).toFixed(1)}px;color:${w.color}">${esc(w.word)}</span>`).join("");$("#downloadCloud").href=`png/${id}.png`;$("#downloadCloud").download=`nuvem_${id}.png`;let law=D.laws.find(x=>x.id===id);$("#details").innerHTML=law?`<span>${esc(law.regiao)} · ${esc(law.fonte)}</span><a href="${esc(law.url)}" target="_blank" rel="noopener">Acessar texto oficial ↗</a>`:`<span>${c.count} norma(s) neste recorte</span><span>Frequência absoluta após limpeza lexical</span>`}
function showNetwork(id){networkCurrent=id;let c=D.clouds[id],n=D.networks[id].article,colors=['#52b96b','#e3a83b','#5a8ec8','#d36f6f','#8c72b8','#4e9c92'];$("#networkTitle").textContent=c.title;$("#networkLawCount").textContent=c.count;$("#networkNodeCount").textContent=n.nodes.length;$("#netUnits").textContent=n.metrics.units;$("#netDensity").textContent=n.metrics.density;$("#netDegree").textContent=n.metrics.mean_degree;$("#netCommunities").textContent=n.metrics.communities;if(!n.nodes.length){$("#networkSvg").innerHTML='<text x="50%" y="50%" text-anchor="middle">Rede insuficiente para este recorte.</text>';return}let W=900,H=610,node=Object.fromEntries(n.nodes.map(x=>[x.id,x]));let edges=n.edges.map(e=>`<line x1="${node[e.source].x/100*W}" y1="${node[e.source].y/100*H}" x2="${node[e.target].x/100*W}" y2="${node[e.target].y/100*H}" stroke="#9aaba0" stroke-opacity="${.18+.65*e.association}" stroke-width="${1+5*e.association}"><title>${esc(node[e.source].label)} ↔ ${esc(node[e.target].label)} · associação ${e.association}</title></line>`).join('');let maxS=Math.max(...n.nodes.map(x=>x.strength));let nodes=n.nodes.map(x=>{let r=7+15*x.strength/maxS,c=colors[(x.community-1)%colors.length];return `<g transform="translate(${x.x/100*W},${x.y/100*H})"><circle r="${r}" fill="${c}" stroke="#fff" stroke-width="2"><title>${esc(x.label)} · força ${x.strength} · grau ${x.degree} · intermediação ${x.betweenness}</title></circle><text y="${r+14}" text-anchor="middle">${esc(x.label)}</text></g>`}).join('');$("#networkSvg").innerHTML=edges+nodes}
$("#viewType").addEventListener('change',populateChoices);$("#viewChoice").addEventListener('change',e=>e.target.value&&show(e.target.value));$("#lawSearch").addEventListener('input',renderChoices);$("#networkViewType").addEventListener('change',populateNetworkChoices);$("#networkViewChoice").addEventListener('change',e=>e.target.value&&showNetwork(e.target.value));$("#networkLawSearch").addEventListener('input',renderNetworkChoices);populateChoices();populateNetworkChoices();let resizeTimer;window.addEventListener('resize',()=>{clearTimeout(resizeTimer);resizeTimer=setTimeout(()=>show(current),120)});
)---"
writeLines(js,file.path(out_dir,"app.js"),useBytes=TRUE)

html <- r"---(<!doctype html><html lang="pt-BR"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Análise Textual e Redes — RLI Brasil</title><meta name="description" content="Nuvens de palavras e redes de coocorrência das leis brasileiras de inovação"><link rel="preconnect" href="https://fonts.googleapis.com"><link rel="preconnect" href="https://fonts.gstatic.com" crossorigin><link href="https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;500;600;700;800&amp;display=swap" rel="stylesheet"><link rel="stylesheet" href="styles.css"></head><body>
<header class="topbar"><div class="container nav"><a class="brand" href="https://rlibrasil.streamlit.app/" aria-label="Voltar ao RLI Brasil"><img class="brand-logo" src="rli_logo_hero.svg" alt="RLI"><span class="brand-title">Repositório de Leis de Inovação Brasil</span></a><nav class="links"><a href="#metodologia">Sobre</a><a href="#nuvens">Nuvens</a><a href="#redes">Redes</a><a href="#dados">Transparência &amp; dados</a></nav></div></header>
<main><section class="hero"><div class="container hero-inner"><div class="eyebrow">Análise textual das normas</div><h1>Conceitos e Redes das Leis de Inovação</h1><p>Consulta, sistematização e visualização dos termos presentes nos marcos normativos de inovação nos três níveis federativos do Brasil.</p><div class="actions"><a class="btn primary" href="#nuvens">Acessar as análises</a><a class="btn" href="#metodologia">Saiba mais</a></div></div></section>
<section class="about" id="metodologia"><div class="container"><div class="eyebrow">Sobre a análise</div><h2 class="section-title">O que a nuvem de palavras revela?</h2><p class="lead">A nuvem oferece uma visualização sintética do vocabulário de cada norma. Os textos passam por tratamento lexical em R, com padronização dos termos e retirada de números, marcações jurídicas, palavras funcionais e resíduos de captura. O tamanho de cada palavra corresponde à frequência com que ela aparece no recorte selecionado.</p><div class="cards"><article class="card"><h3>Termos recorrentes</h3><p>Identifique os conceitos, instrumentos e atores que recebem maior ênfase em cada marco normativo.</p></article><article class="card"><h3>Recortes comparáveis</h3><p>Consulte uma norma específica ou compare resultados agregados por escopo e região.</p></article><article class="card"><h3>Acesso à íntegra</h3><p>Confira a referência de cada norma e acesse diretamente sua fonte oficial.</p></article></div></div></section>
<section class="explore" id="nuvens"><div class="container"><div class="eyebrow">Nuvens de palavras</div><h2 class="section-title">Escolha o recorte da nuvem</h2><p class="lead">Selecione o escopo, a região ou uma norma específica para visualizar os termos mais recorrentes.</p><div class="explorer"><aside class="sidebar"><h3>Filtros da nuvem</h3><p class="filter-help">Faça sua seleção em etapas simples.</p><div class="field"><label for="viewType">1. Tipo de visualização</label><select id="viewType"><option value="panorama">Visão agregada</option><option value="regiao">Agregado por região</option><option value="lei">Lei individual</option></select></div><div class="field" id="lawSearchWrap" hidden><label for="lawSearch">2. Buscar uma lei</label><input id="lawSearch" type="search" placeholder="Número, nome, estado ou município…" autocomplete="off"></div><div class="field"><label for="viewChoice" id="choiceLabel">2. Escolha a abrangência</label><select id="viewChoice"></select></div><div class="stats"><div class="stat"><b id="lawCount">52</b><span>Leis no recorte</span></div><div class="stat"><b id="termCount">—</b><span>Ocorrências válidas</span></div></div><p class="filter-note">As agregações regionais consideram exclusivamente as leis estaduais.</p></aside><section class="stage"><div class="stage-head"><div><h3 id="cloudTitle"></h3><p id="cloudSub"></p></div><span class="badge" id="badge"></span></div><div class="cloud" id="cloud" aria-label="Nuvem de palavras"></div><div class="details" id="details"></div><a class="btn download-action" id="downloadCloud" href="#" download>Baixar esta nuvem em PNG ↓</a></section></div></div></section>
<section class="network-section" id="redes"><div class="container"><div class="eyebrow">Sobre a análise</div><h2 class="section-title">O que a rede de coocorrência revela?</h2><p class="lead">A rede mostra quais conceitos aparecem associados dentro das leis. Dois termos são conectados quando estão presentes no mesmo artigo, adotado como unidade de contexto por representar uma unidade normativa com significado próprio.</p><div class="cards network-cards"><article class="card"><h3>Nós e conexões</h3><p>Cada nó representa um termo. Nós maiores têm maior força, enquanto conexões mais espessas indicam associações mais intensas.</p></article><article class="card"><h3>Comunidades temáticas</h3><p>As cores organizam termos com padrões semelhantes de associação, facilitando a identificação de grupos conceituais.</p></article><article class="card"><h3>Comparabilidade</h3><p>A associação de Ochiai normaliza as coocorrências e reduz o efeito das diferenças de tamanho entre os documentos.</p></article></div><div class="method-note"><b>Como interpretar.</b> O painel apresenta número de artigos analisados, densidade, grau médio e comunidades. A posição dos termos deriva das distâncias da rede; os indicadores disponíveis incluem grau, força e intermediação.</div><div class="result-label">Resultado da análise</div><div class="explorer network-explorer"><aside class="sidebar"><h3>Filtros da rede</h3><p class="filter-help">Esta seleção é independente da nuvem de palavras.</p><div class="field"><label for="networkViewType">1. Tipo de visualização</label><select id="networkViewType"><option value="panorama">Visão agregada</option><option value="regiao">Agregado por região</option><option value="lei">Lei individual</option></select></div><div class="field" id="networkLawSearchWrap" hidden><label for="networkLawSearch">2. Buscar uma lei</label><input id="networkLawSearch" type="search" placeholder="Número, nome, estado ou município…" autocomplete="off"></div><div class="field"><label for="networkViewChoice" id="networkChoiceLabel">2. Escolha a abrangência</label><select id="networkViewChoice"></select></div><div class="stats"><div class="stat"><b id="networkLawCount">52</b><span>Leis no recorte</span></div><div class="stat"><b id="networkNodeCount">—</b><span>Termos na rede</span></div></div><p class="filter-note">As agregações regionais consideram exclusivamente as leis estaduais.</p></aside><section class="stage"><div class="stage-head"><div><h3 id="networkTitle"></h3><p>Rede de coocorrência por artigo</p></div><span class="badge">Artigo</span></div><div class="network-stats"><div><b id="netUnits">—</b><span>artigos analisados</span></div><div><b id="netDensity">—</b><span>densidade</span></div><div><b id="netDegree">—</b><span>grau médio</span></div><div><b id="netCommunities">—</b><span>comunidades</span></div></div><div class="network-canvas"><svg id="networkSvg" viewBox="0 0 900 610" role="img" aria-label="Rede de coocorrência de termos"></svg></div></section></div></div></section>
<section class="downloads" id="dados"><div class="container"><div class="eyebrow">Transparência &amp; dados</div><h2 class="section-title">Acesse os dados do repositório</h2><p class="lead">Baixe a planilha estruturada com os metadados das normas ou consulte o corpus com os textos integrais e o índice de fontes.</p><div class="download-grid"><a class="download-card" href="leis_inovacao_base.xlsx" download><b>Planilha das leis</b><span>Metadados comparáveis e fontes em XLSX ↓</span></a><a class="download-card" href="leis_inovacao_textos_integrais.zip" download><b>Corpus textual</b><span>Textos integrais e índice em ZIP ↓</span></a></div><div class="return-rli"><p>Consulte o mapa interativo e navegue pelos marcos normativos cadastrados no repositório principal.</p><a class="btn primary" href="https://rlibrasil.streamlit.app/">Voltar ao RLI Brasil ↗</a></div></div></section></main>
<footer class="footer"><div class="container"><b>Repositório de Leis de Inovação Brasil</b><p>Consulta, sistematização e acesso aos marcos normativos de inovação.</p></div></footer><script src="data.js"></script><script src="app.js"></script></body></html>)---"
writeLines(html,file.path(out_dir,"index.html"),useBytes=TRUE)

write.csv(index, file.path(out_dir,"dados_leis.csv"), row.names=FALSE, fileEncoding="UTF-8")
write.csv(dictionary, dict_path, row.names=FALSE, fileEncoding="UTF-8")
if (normalizePath(script_path,mustWork=FALSE)!=normalizePath(file.path(out_dir,"build_site.R"),mustWork=FALSE)) file.copy(script_path, file.path(out_dir,"build_site.R"), overwrite=TRUE)
if (!packaged) {
  file.copy("/Users/mac/Downloads/observatorio_leis_inovacao_app_ready (1).xlsx",file.path(out_dir,"leis_inovacao_base.xlsx"),overwrite=TRUE)
  file.copy(file.path(root,"outputs/leis_inovacao_textos_integrais.zip"),file.path(out_dir,"leis_inovacao_textos_integrais.zip"),overwrite=TRUE)
  file.copy("/Users/mac/Downloads/rli_python/assets/rli_logo_hero.svg",file.path(out_dir,"rli_logo_hero.svg"),overwrite=TRUE)
}
# Tabelas analíticas abertas para auditoria e reprodução externa.
node_rows<-list();edge_rows<-list();for(id in names(networks))for(unit in "article"){
  nn<-networks[[id]][[unit]]$nodes;ee<-networks[[id]][[unit]]$edges
  if(length(nn))node_rows[[length(node_rows)+1]]<-do.call(rbind,lapply(nn,function(x)data.frame(recorte=id,unidade=unit,termo=x$label,frequencia=x$frequency,grau=x$degree,forca=x$strength,intermediacao=x$betweenness,comunidade=x$community)))
  if(length(ee)){labs<-vapply(nn,function(x)x$label,character(1));edge_rows[[length(edge_rows)+1]]<-do.call(rbind,lapply(ee,function(x)data.frame(recorte=id,unidade=unit,termo_a=labs[x$source],termo_b=labs[x$target],coocorrencias=x$count,associacao_ochiai=x$association)))}
}
write.csv(do.call(rbind,node_rows),file.path(out_dir,"rede_nos.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(do.call(rbind,edge_rows),file.path(out_dir,"rede_arestas.csv"),row.names=FALSE,fileEncoding="UTF-8")
audit<-sort(table(unlist(tokens)),decreasing=TRUE);write.csv(data.frame(termo=names(audit),frequencia=as.integer(audit)),file.path(out_dir,"auditoria_vocabulario.csv"),row.names=FALSE,fileEncoding="UTF-8")
writeLines(c("RLI — Análise textual e redes de coocorrência", "", "1. Abra o projeto no RStudio.", "2. Execute: source('build_site.R')", "3. Abra index.html em um navegador.", "", "Produtos: nuvens, redes de coocorrência por artigo, métricas de nós e arestas, auditoria lexical e site interativo.", "Dependência: jsonlite."), file.path(out_dir,"LEIA-ME.txt"))
cat(sprintf("Geradas %d nuvens e %d redes por artigo (%d individuais, %d agregadas).\n",length(clouds),length(networks),nrow(index),length(clouds)-nrow(index)))
