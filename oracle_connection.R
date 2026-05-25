# 1. Installation du package (à faire une seule fois)
install.packages("RJDBC")

library(RJDBC)

# 2. Initialiser le pilote en indiquant le chemin du fichier .jar téléchargé
chemin_jar <- "/opt/oracle/ojdbc11.jar" # Mettez le chemin exact (ex: "/home/yoda/projets/ojdbc11.jar")

jdbc_driver <- JDBC(
  driverClass = "oracle.jdbc.OracleDriver",
  classPath   = chemin_jar
)

# 3. Définir les informations du serveur distant
host <- "****"      # IP ou nom de domaine du serveur distant
port <- "****"              # Port d'écoute Oracle
service <- "****"           # Nom du service de la base de données distante

# 4. Construire l'URL de connexion (Syntaxe "Thin client" d'Oracle)
url_connexion <- paste0("jdbc:oracle:thin:@//", host, ":", port, "/", service)

# 5. Se connecter à la base de données distante
con <- dbConnect(
  jdbc_driver,
  url_connexion,
  user     = "****",
  password = "****"
)

# --- VOTRE TRAVAIL SUR LA BASE DE DONNÉES ---

# Exemple : Lister les tables accessibles
print(dbListTables(con))

# Exemple : Exécuter une requête et récupérer les données dans un DataFrame R
donnees <- dbGetQuery(con, "SELECT * FROM GEOGRAPHY WHERE ROWNUM <= 10")
print(head(donnees))

# 6. Toujours fermer la connexion à la fin du script
dbDisconnect(con)
