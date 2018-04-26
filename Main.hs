import Data.List
import System.Environment

data Fasta = Fasta String String String

fasta_name (Fasta name _ _) = name
fasta_comment (Fasta _ comment _) = comment
fasta_data (Fasta _ _ dat) = dat

instance Show Fasta where
    show f = ">" ++ (fasta_name f) ++" " ++ (fasta_comment f) ++ "\n" ++ (fasta_data f) ++ "\n"
                             
parse_fasta ([]:lines)          = error "FASTA Record starts with a blank line!"
parse_fasta (('>':line):lines)  = Fasta name comment body where
    (name,rest) = span (/= ' ') line
    comment = dropWhile (==' ') rest
    body = concatMap (filter (/= ' ')) lines
parse_fasta x  = error ("FASTA Record does not start with '>'!:\n"++show x)
                         
check [] = False
check ('>':_) = False;
check _ = True

parse_first_fasta ([]:[])      = (Nothing,[])
parse_first_fasta ([]:lines)   = parse_first_fasta lines
parse_first_fasta (header:lines) = (Just $ parse_fasta (header:body), rest) where (body,rest) = span check lines

parse_fastas []    = []
parse_fastas lines = case parse_first_fasta lines of ((Just first),rest) -> (first:parse_fastas rest)
                                                     (Nothing,_) -> []

read_fasta filename = do
  content <- readFile filename
  return $ parse_fastas (lines content)
                       
main = do
  args <- getArgs
  let filename = args!!0
  fastas <- read_fasta filename
  putStr $ concatMap show fastas
