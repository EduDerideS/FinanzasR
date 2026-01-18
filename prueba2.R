#Este es un archivo de prueba

install.packages(c("usethis", "renv", "gh"))
usethis::use_git()
usethis::use_github()
gh::gh_whoami()

# NO pegar tokens aquí
token <- Sys.getenv("GITHUB_PAT")

gitcreds::gitcreds_delete()
gitcreds::gitcreds_set()

#Listo