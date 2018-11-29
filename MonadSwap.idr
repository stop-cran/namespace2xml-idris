module MonadSwap


%access public export
%default total


interface Monad m => MonadSwap (m : Type -> Type) where
    swap : Monad n => m (n a) -> n (m a)

MonadSwap Maybe where
    swap Nothing = pure Nothing
    swap (Just x) = map Just x 

MonadSwap (Either a) where
    swap x@(Left _) = pure x
    swap (Right x) = map Right x
    
MonadSwap List where
    swap [] = pure []
    swap (x :: xs) = [| x :: swap xs |]

