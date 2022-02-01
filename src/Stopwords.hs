{- |
SPDX-License-Identifier: CC-BY-NC-4.0

Spanish Stopwords by Alireza Savand. See https://github.com/Alir3z4/stop-words
-}

module Stopwords where


stopwords = ["a",
    "actualmente",
    "adelante",
    "además",
    "afirmó",
    "agregó",
    "ahora",
    "ahí",
    "al",
    "algo",
    "alguna",
    "algunas",
    "alguno",
    "algunos",
    "algún",
    "alrededor",
    "ambos",
    "ampleamos",
    "ante",
    "anterior",
    "antes",
    "apenas",
    "aproximadamente",
    "aquel",
    "aquellas",
    "aquellos",
    "aqui",
    "aquí",
    "arriba",
    "aseguró",
    "así",
    "atras",
    "aunque",
    "ayer",
    "añadió",
    "aún",
    "bajo",
    "bastante",
    "bien",
    "buen",
    "buena",
    "buenas",
    "bueno",
    "buenos",
    "cada",
    "casi",
    "cerca",
    "cierta",
    "ciertas",
    "cierto",
    "ciertos",
    "cinco",
    "comentó",
    "como",
    "con",
    "conocer",
    "conseguimos",
    "conseguir",
    "considera",
    "consideró",
    "consigo",
    "consigue",
    "consiguen",
    "consigues",
    "contra",
    "cosas",
    "creo",
    "cual",
    "cuales",
    "cualquier",
    "cuando",
    "cuanto",
    "cuatro",
    "cuenta",
    "cómo",
    "da",
    "dado",
    "dan",
    "dar",
    "de",
    "debe",
    "deben",
    "debido",
    "decir",
    "dejó",
    "del",
    "demás",
    "dentro",
    "desde",
    "después",
    "dice",
    "dicen",
    "dicho",
    "dieron",
    "diferente",
    "diferentes",
    "dijeron",
    "dijo",
    "dio",
    "donde",
    "dos",
    "durante",
    "e",
    "ejemplo",
    "el",
    "ella",
    "ellas",
    "ello",
    "ellos",
    "embargo",
    "empleais",
    "emplean",
    "emplear",
    "empleas",
    "empleo",
    "en",
    "encima",
    "encuentra",
    "entonces",
    "entre",
    "era",
    "erais",
    "eramos",
    "eran",
    "eras",
    "eres",
    "es",
    "esa",
    "esas",
    "ese",
    "eso",
    "esos",
    "esta",
    "estaba",
    "estabais",
    "estaban",
    "estabas",
    "estad",
    "estada",
    "estadas",
    "estado",
    "estados",
    "estais",
    "estamos",
    "estan",
    "estando",
    "estar",
    "estaremos",
    "estará",
    "estarán",
    "estarás",
    "estaré",
    "estaréis",
    "estaría",
    "estaríais",
    "estaríamos",
    "estarían",
    "estarías",
    "estas",
    "este",
    "estemos",
    "esto",
    "estos",
    "estoy",
    "estuve",
    "estuviera",
    "estuvierais",
    "estuvieran",
    "estuvieras",
    "estuvieron",
    "estuviese",
    "estuvieseis",
    "estuviesen",
    "estuvieses",
    "estuvimos",
    "estuviste",
    "estuvisteis",
    "estuviéramos",
    "estuviésemos",
    "estuvo",
    "está",
    "estábamos",
    "estáis",
    "están",
    "estás",
    "esté",
    "estéis",
    "estén",
    "estés",
    "ex",
    "existe",
    "existen",
    "explicó",
    "expresó",
    "fin",
    "fue",
    "fuera",
    "fuerais",
    "fueran",
    "fueras",
    "fueron",
    "fuese",
    "fueseis",
    "fuesen",
    "fueses",
    "fui",
    "fuimos",
    "fuiste",
    "fuisteis",
    "fuéramos",
    "fuésemos",
    "gran",
    "grandes",
    "gueno",
    "ha",
    "haber",
    "habida",
    "habidas",
    "habido",
    "habidos",
    "habiendo",
    "habremos",
    "habrá",
    "habrán",
    "habrás",
    "habré",
    "habréis",
    "habría",
    "habríais",
    "habríamos",
    "habrían",
    "habrías",
    "habéis",
    "había",
    "habíais",
    "habíamos",
    "habían",
    "habías",
    "hace",
    "haceis",
    "hacemos",
    "hacen",
    "hacer",
    "hacerlo",
    "haces",
    "hacia",
    "haciendo",
    "hago",
    "han",
    "has",
    "hasta",
    "hay",
    "haya",
    "hayamos",
    "hayan",
    "hayas",
    "hayáis",
    "he",
    "hecho",
    "hemos",
    "hicieron",
    "hizo",
    "hoy",
    "hube",
    "hubiera",
    "hubierais",
    "hubieran",
    "hubieras",
    "hubieron",
    "hubiese",
    "hubieseis",
    "hubiesen",
    "hubieses",
    "hubimos",
    "hubiste",
    "hubisteis",
    "hubiéramos",
    "hubiésemos",
    "hubo",
    "igual",
    "incluso",
    "indicó",
    "informó",
    "intenta",
    "intentais",
    "intentamos",
    "intentan",
    "intentar",
    "intentas",
    "intento",
    "ir",
    "junto",
    "la",
    "lado",
    "largo",
    "las",
    "le",
    "les",
    "llegó",
    "lleva",
    "llevar",
    "lo",
    "los",
    "luego",
    "lugar",
    "manera",
    "manifestó",
    "mayor",
    "me",
    "mediante",
    "mejor",
    "mencionó",
    "menos",
    "mi",
    "mientras",
    "mio",
    "mis",
    "misma",
    "mismas",
    "mismo",
    "mismos",
    "modo",
    "momento",
    "mucha",
    "muchas",
    "mucho",
    "muchos",
    "muy",
    "más",
    "mí",
    "mía",
    "mías",
    "mío",
    "míos",
    "nada",
    "nadie",
    "ni",
    "ninguna",
    "ningunas",
    "ninguno",
    "ningunos",
    "ningún",
    "no",
    "nos",
    "nosotras",
    "nosotros",
    "nuestra",
    "nuestras",
    "nuestro",
    "nuestros",
    "nueva",
    "nuevas",
    "nuevo",
    "nuevos",
    "nunca",
    "o",
    "ocho",
    "os",
    "otra",
    "otras",
    "otro",
    "otros",
    "para",
    "parece",
    "parte",
    "partir",
    "pasada",
    "pasado",
    "pero",
    "pesar",
    "poca",
    "pocas",
    "poco",
    "pocos",
    "podeis",
    "podemos",
    "poder",
    "podria",
    "podriais",
    "podriamos",
    "podrian",
    "podrias",
    "podrá",
    "podrán",
    "podría",
    "podrían",
    "poner",
    "por",
    "por qué",
    "porque",
    "posible",
    "primer",
    "primera",
    "primero",
    "primeros",
    "principalmente",
    "propia",
    "propias",
    "propio",
    "propios",
    "próximo",
    "próximos",
    "pudo",
    "pueda",
    "puede",
    "pueden",
    "puedo",
    "pues",
    "que",
    "quedó",
    "queremos",
    "quien",
    "quienes",
    "quiere",
    "quién",
    "qué",
    "realizado",
    "realizar",
    "realizó",
    "respecto",
    "sabe",
    "sabeis",
    "sabemos",
    "saben",
    "saber",
    "sabes",
    "se",
    "sea",
    "seamos",
    "sean",
    "seas",
    "segunda",
    "segundo",
    "según",
    "seis",
    "ser",
    "seremos",
    "será",
    "serán",
    "serás",
    "seré",
    "seréis",
    "sería",
    "seríais",
    "seríamos",
    "serían",
    "serías",
    "seáis",
    "señaló",
    "si",
    "sido",
    "siempre",
    "siendo",
    "siete",
    "sigue",
    "siguiente",
    "sin",
    "sino",
    "sobre",
    "sois",
    "sola",
    "solamente",
    "solas",
    "solo",
    "solos",
    "somos",
    "son",
    "soy",
    "su",
    "sus",
    "suya",
    "suyas",
    "suyo",
    "suyos",
    "sí",
    "sólo",
    "tal",
    "también",
    "tampoco",
    "tan",
    "tanto",
    "te",
    "tendremos",
    "tendrá",
    "tendrán",
    "tendrás",
    "tendré",
    "tendréis",
    "tendría",
    "tendríais",
    "tendríamos",
    "tendrían",
    "tendrías",
    "tened",
    "teneis",
    "tenemos",
    "tener",
    "tenga",
    "tengamos",
    "tengan",
    "tengas",
    "tengo",
    "tengáis",
    "tenida",
    "tenidas",
    "tenido",
    "tenidos",
    "teniendo",
    "tenéis",
    "tenía",
    "teníais",
    "teníamos",
    "tenían",
    "tenías",
    "tercera",
    "ti",
    "tiempo",
    "tiene",
    "tienen",
    "tienes",
    "toda",
    "todas",
    "todavía",
    "todo",
    "todos",
    "total",
    "trabaja",
    "trabajais",
    "trabajamos",
    "trabajan",
    "trabajar",
    "trabajas",
    "trabajo",
    "tras",
    "trata",
    "través",
    "tres",
    "tu",
    "tus",
    "tuve",
    "tuviera",
    "tuvierais",
    "tuvieran",
    "tuvieras",
    "tuviese",
    "tuvieron",
    "tuviesen",
    "tuvieseis",
    "tuvimos",
    "tuvieses",
    "tuvisteis",
    "tuviste",
    "tuviésemos",
    "tuviéramos",
    "tuya",
    "tuvo",
    "tuyo",
    "tuyas",
    "tú",
    "tuyos",
    "un",
    "ultimo",
    "unas",
    "una",
    "unos",
    "uno",
    "usais",
    "usa",
    "usan",
    "usamos",
    "usas",
    "usar",
    "usted",
    "uso",
    "vais",
    "va",
    "vamos",
    "valor",
    "varias",
    "van",
    "vaya",
    "varios",
    "ver",
    "veces",
    "verdadera",
    "verdad",
    "vez",
    "verdadero",
    "vosotros",
    "vosotras",
    "vuestra",
    "voy",
    "vuestro",
    "vuestras",
    "y",
    "vuestros",
    "yo",
    "ya",
    "éramos",
    "él",
    "éstas",
    "ésta",
    "éstos",
    "éste",
    "últimas",
    "última",
    "últimos",
    "último"]