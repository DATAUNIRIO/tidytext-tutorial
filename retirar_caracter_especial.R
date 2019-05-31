##================================================================================================
##                                                                                              
##    Nome: Retirar caracter especial                                           
##                                                    
##    fontes originais/sites:
##    http://www.thomazrossito.com.br/retirar-acentos-de-um-data-frame-com-a-linguagem-r/                                                
##    https://gomesfellipe.github.io/post/2017-12-17-string/string/                                                                                                                                                 
##    
##    prof. Steven Dutt-Ross                          
##    UNIRIO           
##================================================================================================


rm_accent <- function(str,pattern="all") {
    if(!is.character(str))
        str <- as.character(str)
    pattern <- unique(pattern)
    if(any(pattern=="Ç"))
        pattern[pattern=="Ç"] <- "ç"
    symbols <- c(
        acute = "áéíóúÁÉÍÓÚýÝ",
        grave = "àèìòùÀÈÌÒÙ",
        circunflex = "âêîôûÂÊÎÔÛ",
        tilde = "ãõÃÕñÑ",
        umlaut = "äëïöüÄËÏÖÜÿ",
        cedil = "çÇ"
    )
    nudeSymbols <- c(
        acute = "aeiouAEIOUyY",
        grave = "aeiouAEIOU",
        circunflex = "aeiouAEIOU",
        tilde = "aoAOnN",
        umlaut = "aeiouAEIOUy",
        cedil = "cC"
    )
    accentTypes <- c("´","`","^","~","¨","ç")
    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
        return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
    for(i in which(accentTypes%in%pattern))
        str <- chartr(symbols[i],nudeSymbols[i], str)
    return(str)
}


