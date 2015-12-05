package models

import sorm._

object DB extends Instance(
  entities = Set(Entity[User](),
    Entity[Geocach]()),
  url = "jdbc:h2:mem:test"
)
