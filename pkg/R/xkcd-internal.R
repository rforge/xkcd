.ess_dbg_getTracedAndDebugged <-
function(){
    tr_state <- tracingState(FALSE)
    on.exit(tracingState(tr_state))
    generics <- methods::getGenerics()
    all_traced <- c()
    for(i in seq_along(generics)){
        genf <- methods::getGeneric(generics[[i]], package=generics@package[[i]])
        if(!is.null(genf)){ ## might happen !! v.2.13
            menv <- methods::getMethodsForDispatch(genf)
            traced <- unlist(eapply(menv, is, 'traceable', all.names=TRUE))
            if(length(traced) && any(traced))
                all_traced <- c(paste(generics[[i]],':', names(traced)[traced],sep=''), all_traced)
            if(!is.null(tfn<-getFunction(generics[[i]], mustFind=FALSE, where = .GlobalEnv))&&is(tfn,  'traceable')) # if the default is traced,  it does not appear in the menv :()
                all_traced <- c(generics[[i]], all_traced)
        }
    }
    debugged <- apropos('.', mode = 'function')
    ## traced function don't appear here. Not realy needed and would affect performance.
    debugged <- debugged[which(unlist(lapply(debugged, isdebugged) , recursive=FALSE, use.names=FALSE))]
    unique(c(debugged, all_traced))
    }
.ess_dbg_UndebugALL <-
function(funcs){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        invisible(lapply(funcs, .ess_dbg_UntraceOrUndebug))
    }
.ess_dbg_UntraceOrUndebug <-
function(name){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        ## name is a name of a function to be undebugged or has a form name:Class1#Class2#Class3 for traced methods
        name <- strsplit(name, ':', fixed = TRUE)[[1]]
        if(length(name)>1){
            ## a method
            fun <- name[[1]]
            sig <- strsplit(paste(name[-1], collapse=''), '#', fixed=TRUE)[[1]]
            untrace(fun, signature = sig)
        }else{
            ## function
            if(is(getFunction(name), 'traceable'))
                untrace(name)
            else
                undebug(name)
        }
    }
.rutils.help.start <-
function (update=FALSE, remote=NULL) {
    home <- if (is.null(remote)) {
        if (tools:::httpdPort == 0L)
            tools::startDynamicHelp()
        if (tools:::httpdPort > 0L) {
            if (update)
                make.packages.html()
            paste("http://127.0.0.1:", tools:::httpdPort, sep="")
        }
        else stop(".rutils.help.start() requires the HTTP server to be running",
                  call.=FALSE)
    } else remote
    paste(home, "/doc/html/index.html", sep="")
}
