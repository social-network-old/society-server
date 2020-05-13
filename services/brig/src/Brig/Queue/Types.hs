-- This file is part of the Society Server implementation.
--

--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Brig.Queue.Types
  ( Queue (..),
  )
where

import Data.Aeson
import Imports

-- | A remote queue that you can publish to and listen from.
data Queue = StompQueue Text | SqsQueue Text
  deriving (Eq, Show)

instance FromJSON Queue where
  parseJSON = withObject "Queue" $ \o ->
    o .: "queueType" >>= \case
      "stomp" -> StompQueue <$> o .: "queueName"
      "sqs" -> SqsQueue <$> o .: "queueName"
      other -> fail ("unknown 'queueType': " <> other)
