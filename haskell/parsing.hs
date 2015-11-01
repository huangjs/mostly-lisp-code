import Parsing

int :: Parser Int
int = do char '-' 
         n <- nat
         return (-n) 
      +++ nat
      
comment :: Parser ()
comment = do string "--"     
             many (sat (/= '\n'))
             return ()

