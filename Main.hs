import           Data.List
import           System.Environment

data Fasta = Fasta
  { name    :: String
  , comment :: String
  , dat     :: String }

instance Show Fasta where
    show f = ">" ++ (name f) ++" " ++ (comment f) ++ "\n" ++ (dat f) ++ "\n"

parse_fasta :: [String] -> Fasta
parse_fasta ([]:lines)          = error "FASTA Record starts with a blank line!"
parse_fasta (('>':line):lines)  = Fasta name comment body where
    (name,rest) = span (/= ' ') line
    comment = dropWhile (==' ') rest
    body = concatMap (filter (/= ' ')) lines
parse_fasta x  = error ("FASTA Record does not start with '>'!:\n"++show x)

check []      = False
check ('>':_) = False
check _       = True

parse_first_fasta :: [String] -> (Maybe Fasta, [String])
parse_first_fasta ([]:[])      = (Nothing,[])
parse_first_fasta ([]:lines)   = parse_first_fasta lines
parse_first_fasta (header:lines) = (Just $ parse_fasta (header:body), rest)
  where (body,rest) = span check lines

parse_fastas :: [String] -> [Fasta]
parse_fastas []    = []
parse_fastas lines = case parse_first_fasta lines of
                       ((Just first),rest) -> (first:parse_fastas rest)
                       (Nothing,_)         -> []

parseFastas :: [String] -> [Fasta]
parseFastas [] = []
parseFastas lines =
  -- the predicate to groupBy should probably be pulled out
  -- and not use head directly, i.e. handle empty fastas
  let groups = groupBy (\x y -> head y /= '>') lines
  in map parse_fasta groups

read_fasta filename = do
  content <- readFile filename
  return $ parseFastas (lines content)

main = do
  args <- getArgs
  let filename = args!!0
  fastas <- read_fasta filename
  putStr $ concatMap show fastas
