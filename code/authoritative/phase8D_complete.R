ENGINE_SOURCE <- "#include <Rcpp.h>\n#include <vector>\n#include <queue>\n#include <unordered_map>\n#include <limits>\n#include <algorithm>\n#include <cmath>\nusing namespace Rcpp;\nstruct State {std::unordered_map<int,int> next; int fail=0; bool bad=false; int vertex=-1;};\nstruct Router {\n int n,m; std::vector<int> head,edges,from,to; std::vector<State> ac;\n Router(int N,IntegerVector F,IntegerVector T,List patterns):n(N),m(F.size()),head(N+1,0),edges(m),from(m),to(m),ac(1){\n  if(T.size()!=m||n<1) stop(\"invalid graph\");\n  for(int i=0;i<m;i++){from[i]=F[i]-1;to[i]=T[i]-1;if(from[i]<0||from[i]>=n||to[i]<0||to[i]>=n)stop(\"invalid endpoint\");head[from[i]+1]++;}\n  for(int i=1;i<=n;i++)head[i]+=head[i-1]; auto pos=head; for(int i=0;i<m;i++)edges[pos[from[i]]++]=i;\n  for(int j=0;j<patterns.size();j++){IntegerVector p=patterns[j];if(p.size()<2)stop(\"short pattern\");int s=0,last=-1;for(int x:p){int e=x-1;if(e<0||e>=m||(last>=0&&to[last]!=from[e]))stop(\"invalid pattern\");last=e;auto it=ac[s].next.find(e);if(it==ac[s].next.end()){int k=ac.size();ac[s].next[e]=k;ac.emplace_back();ac[k].vertex=to[e];s=k;}else s=it->second;}ac[s].bad=true;}\n  std::queue<int> q;for(auto kv:ac[0].next)q.push(kv.second);while(!q.empty()){int s=q.front();q.pop();for(auto kv:ac[s].next){int e=kv.first,k=kv.second,f=ac[s].fail;while(f&& !ac[f].next.count(e))f=ac[f].fail;auto it=ac[f].next.find(e);ac[k].fail=it==ac[f].next.end()?0:it->second;ac[k].bad=ac[k].bad||ac[ac[k].fail].bad;q.push(k);}}\n }\n int advance(int s,int e){while(s&&!ac[s].next.count(e))s=ac[s].fail;auto it=ac[s].next.find(e);return it==ac[s].next.end()?0:it->second;}\n NumericVector route(int origin,NumericVector cost,double bound,IntegerVector targets){\n  if(origin<1||origin>n||cost.size()!=m||std::isnan(bound)||bound<0)stop(\"invalid query\");\n  const double inf=std::numeric_limits<double>::infinity();std::vector<double> d(n+ac.size(),inf),best(n,inf);std::vector<unsigned char> wanted(n,0);int remaining=0;\n  for(int t:targets){if(t<1||t>n)stop(\"invalid target\");if(!wanted[t-1]){wanted[t-1]=1;remaining++;}}\n  bool limited=remaining>0;using P=std::pair<double,int>;std::priority_queue<P,std::vector<P>,std::greater<P>> heap;d[origin-1]=0;heap.push({0,origin-1});long long pops=0;\n  while(!heap.empty()){auto p=heap.top();heap.pop();double x=p.first;int key=p.second;if(x!=d[key])continue;if(x>bound)break;if(++pops%100000==0)checkUserInterrupt();int s=key<n?0:key-n;int v=s?ac[s].vertex:key;\n   if(!std::isfinite(best[v])){best[v]=x;if(wanted[v]&&--remaining==0&&limited)break;}\n   for(int i=head[v];i<head[v+1];i++){int e=edges[i];int k=advance(s,e);if(ac[k].bad)continue;double w=cost[e];if(!std::isfinite(w)||w<=0)stop(\"invalid cost\");double y=x+w;int dest=k?n+k:to[e];if(y<=bound&&y<d[dest]){d[dest]=y;heap.push({y,dest});}}\n  }\n  NumericVector ans(targets.size());for(int i=0;i<targets.size();i++)ans[i]=best[targets[i]-1];return ans;\n }\n};\n// [[Rcpp::export]]\nSEXP router_create(int n,IntegerVector from,IntegerVector to,List patterns){XPtr<Router> p(new Router(n,from,to,patterns),true);return p;}\n// [[Rcpp::export]]\nNumericVector router_query(SEXP ptr,int origin,NumericVector cost,double bound,IntegerVector targets){XPtr<Router> p(ptr);return p->route(origin,cost,bound,targets);}\n"
phase8D_run <-
function () 
{
    library(data.table)
    library(digest)
    started <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    p <- function(...) file.path(root, ...)
    gd <- p("04_Spatial_Analysis", "National_Road_Network", "Directed_Graphs")
    cd <- p("04_Spatial_Analysis", "National_Road_Network", "Routing_Costs")
    od <- p("04_Spatial_Analysis", "National_Road_Network", "Routing_Engine")
    dir.create(od, recursive = TRUE, showWarnings = FALSE)
    hash <- function(x) {
        stopifnot(all(file.exists(x)))
        vapply(x, function(f) digest(file = f, algo = "sha256"), character(1), USE.NAMES = FALSE)
    }
    phases <- c("8B2", "8C1", "8C2")
    counts <- c(52L, 21L, 23L)
    gates <- c(36L, 20L, 10L)
    inputs <- rbindlist(lapply(seq_along(phases), function(i) {
        z <- fread(p("12_Reproducibility", paste0("phase", phases[i], "_output_checksum_manifest.csv")))
        q <- fread(p("03_Data_Audit", paste0("phase", phases[i], "_quality_gate.csv")))
        stopifnot(nrow(z) == counts[i], nrow(q) == gates[i], all(q$passed))
        z[, .(relative_path, SHA256)]
    }))
    stopifnot(all(hash(p(inputs$relative_path)) == inputs$SHA256))
    cat("Previous manifests: 96/96 PASS\n")
    if (!requireNamespace("Rcpp", quietly = TRUE)) 
        install.packages("Rcpp", repos = "https://cloud.r-project.org")
    cpp <- file.path(od, "phase8D_engine.cpp")
    writeLines(ENGINE_SOURCE, cpp, useBytes = TRUE)
    tryCatch(Rcpp::sourceCpp(cpp, env = environment()), error = function(e) stop("C++ compilation failed. Install matching Rtools if missing. Original error: ", conditionMessage(e), call. = FALSE))
    cat("C++ engine compiled\n")
    compile_patterns <- function(a, r) {
        s <- r$simple_forbidden_pairs
        patterns <- lapply(seq_len(nrow(s)), function(i) as.integer(c(s$incoming_arc[i], s$outgoing_arc[i])))
        mandatory <- 0L
        for (rule in r$complex_rules) {
            z <- as.integer(rule$arc_ids)
            stopifnot(length(z) >= 3L, all(a$to[z[-length(z)]] == a$from[z[-1L]]))
            if (rule$rule_type == "FORBID_COMPLETE_SEQUENCE") 
                patterns[[length(patterns) + 1L]] <- z
            else if (rule$rule_type == "REQUIRE_CONTINUATION") {
                for (k in seq_len(length(z) - 1L)) {
                  at <- a$to[z[k]]
                  alt <- a$arc_id[a$from == at & a$arc_id != z[k + 1L]]
                  for (e in alt) {
                    patterns[[length(patterns) + 1L]] <- c(z[seq_len(k)], as.integer(e))
                    mandatory <- mandatory + 1L
                  }
                }
            }
            else stop("Unknown rule type")
        }
        keys <- vapply(patterns, paste, character(1), collapse = ",")
        list(patterns = patterns[!duplicated(keys)], mandatory_deviations = mandatory)
    }
    reference <- function(f, t, w, patterns, origin, target, bound = Inf) {
        maxhist <- max(c(1L, lengths(patterns))) - 1L
        queue <- list(list(v = origin, h = integer(), d = 0))
        seen <- new.env(hash = TRUE, parent = emptyenv())
        while (length(queue)) {
            ix <- which.min(vapply(queue, function(x) x$d, numeric(1)))
            x <- queue[[ix]]
            queue[[ix]] <- NULL
            key <- paste(c(x$v, x$h), collapse = ",")
            if (exists(key, seen, inherits = FALSE)) 
                next
            assign(key, TRUE, seen)
            if (x$v == target) 
                return(x$d)
            for (e in which(f == x$v)) {
                h <- c(x$h, e)
                bad <- any(vapply(patterns, function(z) length(h) >= length(z) && identical(as.integer(tail(h, length(z))), as.integer(z)), logical(1)))
                if (!bad && x$d + w[e] <= bound) 
                  queue[[length(queue) + 1L]] <- list(v = t[e], h = tail(h, maxhist), d = x$d + w[e])
            }
        }
        Inf
    }
    equal <- function(x, y) all((is.infinite(x) & is.infinite(y)) | (is.finite(x) & is.finite(y) & abs(x - y) < 1e-08))
    f <- c(1L, 2L, 3L, 2L, 4L, 3L, 4L)
    t <- c(2L, 3L, 5L, 4L, 5L, 2L, 3L)
    w <- c(1, 1, 1, 2, 2, 1, 1)
    designs <- list(list(), list(c(1L, 2L)), list(c(1L, 2L, 3L)), list(c(1L, 4L), c(1L, 2L, 6L)), list(c(1L, 2L, 3L), c(2L, 6L, 4L)))
    tests <- list()
    for (i in seq_along(designs)) {
        pat <- designs[[i]]
        g <- router_create(6L, f, t, pat)
        revg <- router_create(6L, t, f, lapply(pat, rev))
        for (o in 1:6) for (d in 1:6) for (b in c(3, Inf)) {
            x <- router_query(g, o, w, b, as.integer(d))
            y <- reference(f, t, w, pat, o, d, b)
            z <- router_query(revg, d, w, b, as.integer(o))
            tests[[length(tests) + 1L]] <- data.table(design = i, origin = o, target = d, bound = b, forward_reference = equal(x, y), reverse_equivalence = equal(x, z))
        }
    }
    tests <- rbindlist(tests)
    stopifnot(all(tests$forward_reference), all(tests$reverse_equivalence))
    cat("Synthetic comparisons:", nrow(tests), "PASS\n")
    toy <- data.table(arc_id = seq_along(f), from = f, to = t)
    rr <- list(simple_forbidden_pairs = data.table(incoming_arc = integer(), outgoing_arc = integer()), complex_rules = list(list(rule_type = "REQUIRE_CONTINUATION", arc_ids = c(1L, 2L, 3L))))
    pp <- compile_patterns(toy, rr)$patterns
    stopifnot(any(vapply(pp, function(z) identical(z, c(1L, 4L)), logical(1))), any(vapply(pp, function(z) identical(z, c(1L, 2L, 6L)), logical(1))))
    scenarios <- c("CORE_PRIMARY", "EXPANDED", "CORE_STRICT_BARRIERS")
    results <- list()
    audits <- list()
    files <- cpp
    for (nm in scenarios) {
        cat("Testing national graph:", nm, "\n")
        a <- readRDS(file.path(gd, paste0(nm, "_arcs.rds")))
        cst <- readRDS(file.path(cd, paste0(nm, "_arc_travel_costs.rds")))
        r <- readRDS(file.path(cd, paste0(nm, "_turn_rules.rds")))
        v <- readRDS(file.path(gd, paste0(nm, "_vertices.rds")))
        stopifnot(identical(as.integer(a$arc_id), seq_len(nrow(a))), identical(a$arc_id, cst$arc_id), all(is.finite(cst$slow_seconds) & cst$slow_seconds > 0), all(cst$slow_seconds >= cst$baseline_seconds), all(cst$baseline_seconds >= cst$favorable_seconds), all(cst$favorable_seconds > 0))
        cp <- compile_patterns(a, r)
        pat <- cp$patterns
        rp <- lapply(pat, rev)
        f <- as.integer(a$from)
        t <- as.integer(a$to)
        N <- nrow(v)
        g <- router_create(N, f, t, pat)
        gr <- router_create(N, t, f, rp)
        gu <- router_create(N, f, t, list())
        first <- vapply(pat, function(z) z[1L], integer(1))
        last <- vapply(pat, function(z) tail(z, 1L), integer(1))
        pick <- unique(c(seq_len(min(12L, length(pat))), if (length(pat)) tail(seq_along(pat), min(12L, length(pat))) else integer()))
        for (i in pick) {
            o <- f[first[i]]
            d <- t[last[i]]
            b <- 7200
            slow <- router_query(g, o, cst$slow_seconds, b, d)
            base <- router_query(g, o, cst$baseline_seconds, b, d)
            fav <- router_query(g, o, cst$favorable_seconds, b, d)
            revbase <- router_query(gr, d, cst$baseline_seconds, b, o)
            un <- router_query(gu, o, cst$baseline_seconds, b, d)
            stopifnot(equal(base, revbase), slow + 1e-07 >= base, base + 1e-07 >= fav, base + 1e-07 >= un)
            results[[length(results) + 1L]] <- data.table(scenario = nm, pattern_index = i, origin = o, destination = d, slow_seconds = slow, baseline_seconds = base, favorable_seconds = fav, reverse_baseline_seconds = revbase, unrestricted_seconds = un, restricted_increase = if (is.finite(base) && is.finite(un)) 
                base - un
            else NA_real_)
        }
        audit <- data.table(scenario = nm, vertices = N, arcs = nrow(a), simple_pairs = nrow(r$simple_forbidden_pairs), complex_rules = length(r$complex_rules), unresolved_complex = sum(r$complex_audit$status != "UNIQUE_SEQUENCE"), forbidden_patterns = length(pat), mandatory_deviations = cp$mandatory_deviations)
        audits[[nm]] <- audit
        print(audit)
        dest <- file.path(od, paste0(nm, "_routing_patterns.rds"))
        saveRDS(list(scenario = nm, forward_patterns = pat, reverse_patterns = rp, unresolved = r$complex_audit[status != "UNIQUE_SEQUENCE"], arc_count = nrow(a), vertex_count = N), dest)
        files <- c(files, dest)
        rm(g, gr, gu, a, cst, r, v, f, t)
        gc()
    }
    res <- rbindlist(results)
    audit <- rbindlist(audits)
    stopifnot(nrow(audit) == 3L, sum(audit$simple_pairs) == 350L, sum(audit$complex_rules) == 87L, sum(audit$unresolved_complex) == 12L, nrow(res) > 0L)
    checks <- data.table(check = c("Previous 96 manifest entries unchanged", "Previous quality gates passed", "Synthetic forward reference tests", "Synthetic reverse equivalence tests", "Mandatory deviations compiler test", "Three network scenarios", "350 simple pairs preserved", "87 complex rules preserved", "12 unresolved complex records retained", "National sampled forward/reverse and cost order tests"), passed = c(all(hash(p(inputs$relative_path)) == inputs$SHA256), TRUE, all(tests$forward_reference), 
        all(tests$reverse_equivalence), TRUE, nrow(audit) == 3L, sum(audit$simple_pairs) == 350L, sum(audit$complex_rules) == 87L, sum(audit$unresolved_complex) == 12L, nrow(res) > 0L))
    stopifnot(all(checks$passed))
    outputs <- list(`03_Data_Audit/phase8D_synthetic_tests.csv` = tests, `03_Data_Audit/phase8D_quality_gate.csv` = checks, `10_Tables/phase8D_router_summary.csv` = audit, `08_Results/phase8D_sample_routes.csv` = res, `12_Reproducibility/phase8D_modelling_assumptions.csv` = data.table(item = c("Router", "Mandatory rules", "Unresolved rules", "Bounded results", "Travel mode"), treatment = c("Dijkstra with forbidden-sequence automaton", "Every deviation from active compiled prefix forbidden", "12 complex scenario relations not enforced; absent members do not prove irrelevance", 
        "Inf means unreachable within search bound, not necessarily disconnected", "Light motorcar proxy; no connector or congestion delay")))
    for (nm in names(outputs)) fwrite(outputs[[nm]], p(nm))
    files <- c(files, p(names(outputs)))
    script <- p("13_R_Scripts", "phase8D_complete.R")
    code <- c(paste0("ENGINE_SOURCE <- ", paste(deparse(ENGINE_SOURCE, width.cutoff = 500L), collapse = "\n")), "phase8D_run <-", deparse(phase8D_run, width.cutoff = 500L), "phase8D_results <- phase8D_run()")
    parse(text = code)
    writeLines(code, script)
    files <- c(files, script)
    session <- p("12_Reproducibility", "phase8D_R_session_info.txt")
    writeLines(capture.output(sessionInfo()), session)
    files <- c(files, session)
    files <- normalizePath(files, winslash = "/", mustWork = TRUE)
    mf <- data.table(relative_path = substring(files, nchar(root) + 2L), SHA256 = hash(files))
    fwrite(mf, p("12_Reproducibility", "phase8D_output_checksum_manifest.csv"))
    mf[, `:=`(verified, hash(p(relative_path)) == SHA256)]
    stopifnot(all(mf$verified), all(hash(p(inputs$relative_path)) == inputs$SHA256))
    fwrite(mf, p("12_Reproducibility", "phase8D_checksum_recheck.csv"))
    print(audit)
    print(res[, .(queries = .N, finite_baseline = sum(is.finite(baseline_seconds)), finite_increased = sum(restricted_increase > 1e-07, na.rm = TRUE)), by = scenario])
    print(checks)
    cat("PHASE 8D ROUTER TEST PASS\nChecksums:", sum(mf$verified), "/", nrow(mf), "\nElapsed seconds:", as.numeric(difftime(Sys.time(), started, units = "secs")), "\n")
    invisible(list(summary = audit, routes = res, tests = tests, checks = checks, manifest = mf))
}
phase8D_results <- phase8D_run()
