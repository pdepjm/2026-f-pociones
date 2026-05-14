module Library where
import PdePreludat

-- notacion point free

losMayoresA2 :: [Number] -> [Number]
losMayoresA2 = filter (> 2)

-- Sin composicion
calculoLoco :: Number -> Number
calculoLoco n = (((n + 1) * 2) / 3) + 10

-- Con composicion
calculoLoco' :: Number -> Number
calculoLoco' = (+ 10) . (/ 3) . (* 2) . (+ 1)

calculoInsano :: [Number] -> [Number]
calculoInsano = map (\x -> x+1)

sumarVida :: Number -> Heroe  -> Heroe
sumarVida numero = cambiarVida (+ numero)

type Pocion = Heroe -> Heroe

pocionBase :: Pocion
pocionBase = sumarVida 10 

data Heroe = UnHeroe{
    vida :: Number,
    defensa :: Number,
    ataque :: Number
} deriving Show

caballero :: Heroe
caballero = UnHeroe{vida = 1000, defensa = 20, ataque = 50}

poder :: Heroe -> Number
poder heroe = 3 * defensa heroe + ataque heroe + vida heroe / 2

cambiarAtaque :: (Number -> Number) -> Heroe -> Heroe
cambiarAtaque modificacion heroe = heroe{ataque = modificacion (ataque heroe)}

cambiarDefensa :: (Number -> Number) -> Heroe -> Heroe
cambiarDefensa modificacion heroe = heroe{defensa = modificacion (defensa heroe)}

cambiarVida :: (Number -> Number) -> Heroe -> Heroe
cambiarVida modificacion heroe = heroe{vida = modificacion (vida heroe)}


pocionPremium :: Pocion
pocionPremium = pocionBase . pocionBase

-- esto esta mal!!! no funca!
--pocionPremium :: Pocion
--pocionPremium = 2 * pocionBase

-- esto tambien esta mal!!!
--pocionPremium :: Pocion
--pocionPremium = (2 *) . pocionBase

crazyPotion :: Pocion
crazyPotion = cambiarVida (* 1.33) . pocionPremium . cambiarAtaque (* 2)

-- Ambas versiones son validas
agua :: Pocion
agua heroe = heroe

agua' :: Pocion
agua' = id

pocionElite :: Pocion
pocionElite heroe
    | esPoderoso heroe = cambiarDefensa (* 10) heroe
    | otherwise = agua heroe


esPoderoso :: Heroe -> Bool
esPoderoso heroe = poder heroe > 100 

esPoderoso' :: Heroe -> Bool
esPoderoso'  = (> 100) . poder 

pocionArtesanal :: Number -> Pocion
pocionArtesanal unidades = pocionBase . (cambiarAtaque (/ unidades)) . pocionBase

-- Version con lambda
pocionArriesgada :: Pocion
pocionArriesgada =  pocionBase . (cambiarDefensa (\x -> 3) ) . crazyPotion

-- Version con funcion auxiliar
pocionArriesgada2 :: Pocion
pocionArriesgada2 =  pocionBase . (cambiarDefensa siempreTres ) . crazyPotion

siempreTres :: a -> Number
siempreTres _ = 3

licuadoDePociones :: Pocion -> Pocion
licuadoDePociones pocion = pocionArtesanal 10 . pocion . pocionBase

-- Version con un poco de repeticion de logica, no esta muy mal!
pocionGradual :: Pocion
pocionGradual heroe
    | ataque heroe > 100 = aplicarDefensaArtesanal 50 heroe
    | ataque heroe >= 50 = aplicarDefensaArtesanal 30 heroe
    | otherwise = aplicarDefensaArtesanal 10 heroe

aplicarDefensaArtesanal :: Number -> Heroe -> Heroe
aplicarDefensaArtesanal defensa = pocionArtesanal 5 . cambiarDefensa (+ defensa)

-- Version con repeticion de logica, malo!!!! feo!!! no hacer esto!!!
pocionGradual2 :: Pocion
pocionGradual2 heroe
    | ataque heroe > 100 = (pocionArtesanal 5 . cambiarDefensa (+ 50)) heroe
    | ataque heroe >= 50 && ataque heroe <= 100 = (pocionArtesanal 5 . cambiarDefensa (+ 30)) heroe
    | otherwise = (pocionArtesanal 5 . cambiarDefensa (+ 10)) heroe

-- Version sin repeticion de logica, muy bueno!
pocionGradual3 :: Pocion
pocionGradual3 heroe = aplicarDefensaArtesanal (defensaSegun heroe) heroe

defensaSegun :: Heroe -> Number
defensaSegun heroe  
    | ataque heroe > 100 = 50
    | ataque heroe >= 50 = 30 
    | otherwise = 10
