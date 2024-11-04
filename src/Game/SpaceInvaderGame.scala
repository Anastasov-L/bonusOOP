package game

import processing.core.PApplet
import engine.graphics.Point
import scala.math._
import ddf.minim.{Minim, AudioPlayer, AudioSample}

case class Bullet(position: Point) {
  def move(): Bullet = copy(position = Point(position.x + 25, position.y))
}
case class Beam(position: Point, duration: Float = 6.0f) {
  val speed: Float = 8.0f

  def draw(p: PApplet): Unit = {
    p.fill(255, 0, 0, 150)
    p.rect(position.x, position.y, -position.x, 10)
  }

  def move(): Beam = {
    copy(position = Point(position.x - speed, position.y))
  }

  def isExpired(currentTime: Float, beamStartTime: Float): Boolean = {
    currentTime - beamStartTime > duration
  }
}


case class BossBullet(position: Point) {
  val speed: Float = 10.0f

  def move(): BossBullet = {
    copy(position = Point(position.x - speed, position.y))
  }

  def draw(p: PApplet): Unit = {
    p.fill(255, 0, 0)
    p.ellipse(position.x, position.y, 10, 10)
  }
}

case class Ship(position: Point, symbol: String = "<[]>") {
  val moveSpeed: Float = 12.0f

  def moveLeft(): Ship = copy(position = Point(position.x - moveSpeed, position.y))
  def moveRight(): Ship = copy(position = Point(position.x + moveSpeed, position.y))
  def moveUp(): Ship = copy(position = Point(position.x, position.y - moveSpeed))
  def moveDown(): Ship = copy(position = Point(position.x, position.y + moveSpeed))

  def moveNortheast(): Ship = copy(position = Point(position.x + moveSpeed , position.y - moveSpeed ))
  def moveNorthwest(): Ship = copy(position = Point(position.x - moveSpeed , position.y - moveSpeed ))
  def moveSoutheast(): Ship = copy(position = Point(position.x + moveSpeed , position.y + moveSpeed ))
  def moveSouthwest(): Ship = copy(position = Point(position.x - moveSpeed , position.y + moveSpeed ))
}

case class Meteor(position: Point) {

  def draw(p: PApplet): Unit = {
    p.fill(255)
    p.textSize(12)
    val meteorArt =
      """⠀⠀⠀⠀⠀⠀⠀⠀⢀⡠⠔⠚⠉⠩⠍⠩⠍⢩⣶⣦⣤⡀⠀⠀⠀⠀⠀⠀⠀
        |⠀⠀⠀⠀⠀⡠⡲⠑⢈⣨⠵⠊⠁⠀⠀⠀⠈⠲⢌⡻⣿⣶⣄⡀⠀⠀⠀⠀
        |⠀⠀⠀⣠⡾⠊⠈⠉⠉⣑⣀⣀⠀⠀⠀⠀⠀⡶⢄⡈⢻⣿⠟⠻⣄⠀⠀⠀
        |⠀⠀⡐⡑⠁⢀⠏⢠⢞⠕⠚⠉⢻⣏⠀⠀⠀⠑⠀⢱⠀⠉⢇⠀⢹⣦⠀⠀
        |⠀⠰⣼⠀⠀⠀⢰⡎⠁⠀⠀⠀⠀⠁⠀⠀⠀⠀⠀⠃⠀⠀⠈⠘⡟⢿⡇⠀
        |⠀⢷⡿⢰⠓⠀⠀⢣⠀⠀⠀⠀⠀⠀⡄⠀⠀⠀⠀⠐⠀⢄⠄⠄⢣⣸⡿⠀
        |⠘⣸⠁⠸⠔⢀⡀⠀⠳⠦⢤⡶⠂⠀⠀⠀⠀⠀⠀⠀⠀⡀⠣⡆⠀⣿⣷⠂
        |⠀⣿⠀⠀⠀⠈⠁⠀⠀⠀⠀⠓⠀⠀⠀⠀⠀⠀⠀⠐⠁⠀⣀⢀⠿⢿⣿⠀
        |⠀⠸⡇⢀⠀⠀⡀⠀⠀⠀⠀⢄⠀⠀⠀⡄⠀⠀⠀⠀⢀⡞⢀⠄⠀⣼⡇⠀
        |⠀⠀⠻⡌⢆⠰⡠⠐⠈⠀⣤⠜⠒⢢⠀⠀⠀⠢⠄⢀⣈⣄⢾⢴⡿⡟⠀⠀
        |⠀⠀⠀⠹⣌⡿⢄⠀⠀⠀⠣⣄⢀⠶⠃⠀⢀⣀⣀⣤⣿⢿⣶⣯⠊⠀⠀⠀
        |⠀⠀⠀⠀⠈⠛⢷⣝⡢⢔⡀⠈⠂⠤⠤⠀⢉⣹⠿⣫⣴⡿⠛⠁⠀⠀⠀⠀
        |⠀⠀⠀⠀⠀⠀⠀⠉⠛⠲⠤⣷⣦⣶⣶⣞⣛⠛⠿⠛⠋⠀⠀⠀⠀⠀⠀⠀
        |⠀⠀""".stripMargin
    p.text(meteorArt, position.x, position.y)
  }
}
case class Boss(
                 position: Point,
                 hasMoved: Boolean = false,
                 health: Int = 100,
                 defeated: Boolean = false,
                 aligning: Boolean = false,
                 alignStartTime: Float = 0,
                 lastAlignTime: Float = 0,
                 movingUp: Boolean = true,
                 moveTargetY: Float = 0,
                 moveCenterY: Float = 300
               ) {
  def draw(p: PApplet): Unit = {
    p.fill(255)
    p.textSize(35)
    val bossArt =
      """. _________________________
        |                    __,' __`.                _..----....____
        |        __...--.'``;.   ,.   ;``--..__     .'    ,-._    _.-'
        |  _..-''-------'   `'   `'   `'     O ``-''._   (,;') _,'
        |,'________________                          \`-._`-','
        | `._              ```````````------...___   '-.._'-:
        |    ```--.._      ,.                     ````--...__\-.
        |            `.--. `-`                       ____    |  |`
        |              `. `.                       ,'`````.  ;  ;`
        |                `._`.        __________   `.      \'__/`
        |                   `-:._____/______/___/____`.     \  `
        |                               |       `._    `.    \
        |                               `._________`-.   `.   `.___
        |                                             SSt  `------'""".stripMargin
    if (!defeated) {
      p.text(bossArt, position.x, position.y)
    } else {
      p.pushMatrix()
      p.translate(position.x + 220, position.y + 50)
      p.rotate(PApplet.radians(-45))
      p.text(bossArt, -220, -50)
      p.popMatrix()
    }


  }

  def move(shipY: Float, currentTime: Float, screenHeight: Float): Boss = {
    val minY = -150
    val maxY = screenHeight - 200

    if (defeated) {
      if (position.y < 1600) {
        copy(position = Point(position.x - 3, position.y + 5))
      } else {
        this.copy(position = Point(position.x, 1600))
      }
    } else if (position.x > 800) {
      copy(position = Point(position.x - 3, position.y))
    } else if (aligning && currentTime - alignStartTime <= 3) {
      val newY = if (position.y < shipY - 150) position.y + 5
      else if (position.y > shipY - 500) position.y - 5
      else position.y
      copy(position = Point(position.x, clamp(newY, minY, maxY)))
    } else if (aligning && currentTime - alignStartTime > 3) {
      val centerY = moveCenterY
      val moveBackY = if (position.y < centerY) position.y + 2 else position.y - 2

      copy(position = Point(position.x, clamp(moveBackY, minY, maxY)), aligning = false)
    } else if (currentTime - lastAlignTime >= 10) {
      startAligning(currentTime)
    } else {
      if (moveTargetY == 0) {
        val randomDistance = 100 + (Math.random() * 500).toFloat  // Random distance between 100 and 500 pixels
        val newTargetY = if (Math.random() > 0.5) position.y - randomDistance else position.y + randomDistance
        copy(moveTargetY = clamp(newTargetY, minY, maxY))
      } else {
        val newY = if (position.y < moveTargetY) position.y + 2 else position.y - 2

        if (Math.abs(newY - moveTargetY) <= 2) {
          val adjustedCenterY = if (position.y < moveCenterY) position.y + 2 else position.y - 2
          copy(position = Point(position.x, clamp(adjustedCenterY, minY, maxY)), moveTargetY = 0)
        } else {
          copy(position = Point(position.x, clamp(newY, minY, maxY)))
        }
      }
    }
  }

  def startAligning(currentTime: Float): Boss = {
    copy(aligning = true, alignStartTime = currentTime, lastAlignTime = currentTime)
  }

  def canFire(): Boolean = !defeated && position.x <= 800

  def isHit(bulletPosition: Point): Boolean = {
    val hitboxX = position.x + 220
    val hitboxY = position.y + 50
    val hitboxWidth = 400
    val hitboxHeight = 400

    bulletPosition.x >= hitboxX &&
      bulletPosition.x <= hitboxX + hitboxWidth &&
      bulletPosition.y >= hitboxY &&
      bulletPosition.y <= hitboxY + hitboxHeight
  }

  def takeDamage(amount: Int): Boss = {
    val newHealth = Math.max(0, health - amount)
    if (newHealth == 0) {
      copy(health = newHealth, defeated = true)
    } else {
      copy(health = newHealth)
    }
  }

  private def clamp(value: Float, min: Float, max: Float): Float = {
    math.max(min, math.min(value, max))
  }
}






  case class Explosion(position: Point, stage: Int) {
  def draw(p: PApplet): Unit = {
    p.fill(255, 0, 0)
    p.textSize(12)
    val explosionStages = Array(
      "[]",
      "[ ]",
      "[  ]",
      "[   ]",
      "[    ]",
      "[   ]",
      "[ ]",
      "[]"
    )

    if (stage < explosionStages.length) {
      p.text(explosionStages(stage), position.x, position.y)
    }
  }

  def nextStage(): Explosion = copy(stage = stage + 1)
}
case class GameState(
                      ship: Ship,
                      bullets: List[Bullet] = List(),
                      meteors: List[Meteor] = List(),
                      explosions: List[Explosion] = List(),
                      previousStates: List[GameState] = List(),
                      maxHealth: Int = 10,
                      shield: Int = 10,
                      maxShield: Int = 10,
                      bulletHits: Int = 0,
                      var health: Int = 10,
                      var hit: Boolean = false,
                      hits: Int = 0,
                      frameCount: Int = 0,
                      radioCharCount: Int = 0,
                      boss: Option[Boss] = None,
                      bossBullets: List[BossBullet] = List(),
                      bossLastShotTime: Float = 0,
                      bulletCycleCount: Int = 0,
                      beamActive: Boolean = false,
                      beamStartTime: Float = 0,
                      beamDuration: Float = 3.0f,
                      beamCooldown: Float = 10.0f,
                      meteorDestroyedCount: Int = 0,
                      var gameOverSoundPlayed: Boolean = false
                    ) {
  def updateBeamDamage(): GameState = {
    val currentTime = frameCount / 60.0f


    val isTimeForBeam = !beamActive && currentTime - beamStartTime >= beamCooldown && boss.exists(!_.defeated)


    val updatedBeamActive = if (isTimeForBeam) {
      true
    } else if (beamActive && currentTime - beamStartTime > beamDuration) {

      false
    } else {
      beamActive
    }

    val newBeamStartTime = if (isTimeForBeam) currentTime else beamStartTime

    val beam = boss.map(b => Beam(Point(b.position.x, b.position.y + 200)))
    val shipInBeam = beam.exists(b => checkBeamCollisionWithShip(b, ship))

    val newHealth = if (shipInBeam && shield <= 0) max(health - 1, 0) else health
    val newShield = if (shipInBeam && shield > 0) max(shield - 1, 0) else shield

    copy(beamActive = updatedBeamActive, beamStartTime = newBeamStartTime, health = newHealth, shield = newShield)
  }

  def checkBeamCollisionWithShip(beam: Beam, ship: Ship): Boolean = {
    ship.position.y < beam.position.y + 10 &&
      ship.position.y + 70 > beam.position.y &&
      beam.position.x > ship.position.x && beamActive
  }


  def spawnBossIfNeeded(): GameState = {
    if (meteors.isEmpty && boss.isEmpty) {
      copy(boss = Some(Boss(Point(2000, 300))))
    } else {
      this
    }
  }

  def checkCollision(bullets: List[Bullet], meteors: List[Meteor]): Boolean = {
    meteors.exists { meteor =>
      val centerX = meteor.position.x + 10 * 12
      val centerY = meteor.position.y + 7 * 12

      val semiMajorAxis = (13 * 14 + 10) / 2.0
      val semiMinorAxis = (13 * 14 + 10) / 2.0

      val hits = bullets.count { bullets =>
        val bulletX = bullets.position.x
        val bulletY = bullets.position.y

        val ellipseEquation = Math.pow((bulletX - centerX) / semiMajorAxis, 2) +
          Math.pow((bulletY - centerY) / semiMinorAxis, 2)

        ellipseEquation < 1.0 + 0.05
      }
      if (hits >= 1) {
        true
      } else {
        false
      }
    }
  }

  def isCircleColliding(circle1: Point, radius1: Float, circle2: Point, radius2: Float): Boolean = {
    val distanceX = circle1.x - circle2.x
    val distanceY = circle1.y - circle2.y
    val distanceSquared = distanceX * distanceX + distanceY * distanceY
    val radiusSum = radius1 + radius2
    distanceSquared <= radiusSum * radiusSum
  }

  def checkCollisionShip(ship: Ship, meteors: List[Meteor], explosionSound: AudioSample): Boolean = {
    meteors.exists { meteor =>
      val shipCenter = Point(ship.position.x + 5 * 12, ship.position.y + 5 * 12)
      val shipRadius = 70

      val meteorCenter = Point(meteor.position.x + 10 * 12, meteor.position.y + 7 * 12)
      val meteorRadius = 80

      if (isCircleColliding(shipCenter, shipRadius, meteorCenter, meteorRadius)) {
        hit = true
        explosionSound.trigger()
        true
      } else {
        false
      }
    }
  }


  def updateMeteor(width: Float, height: Float, health: Int, hit: Boolean, explosionSound: AudioSample): GameState = {
    val newMeteors = meteors.map { meteor =>
      if (meteor.position.x < -100 || checkCollisionShip(ship, List(meteor), explosionSound)) {
        None
      } else {
        val newMeteorPosition = meteor.position.copy(x = meteor.position.x - 35)
        Some(meteor.copy(position = newMeteorPosition))
      }
    }
    val hitty = meteors.exists(meteor => checkCollisionShip(ship, List(meteor), explosionSound))
    copy(meteors = newMeteors.flatten, hit = hitty)
  }


  def moveLeft(): GameState = {
    val newShip = ship.moveLeft()
    copy(ship = newShip, previousStates = this :: previousStates)
  }

  def moveRight(): GameState = {
    val newShip = ship.moveRight()
    copy(ship = newShip, previousStates = this :: previousStates)
  }

  def moveUp(): GameState = {
    val newShip = ship.moveUp()
    copy(ship = newShip, previousStates = this :: previousStates)
  }

  def moveDown(): GameState = {
    val newShip = ship.moveDown()
    copy(ship = newShip, previousStates = this :: previousStates)
  }

  def moveNortheast(): GameState = {
    val newShip = ship.moveNortheast()
    copy(ship = newShip, previousStates = this :: previousStates)
  }

  def moveNorthwest(): GameState = {
    val newShip = ship.moveNorthwest()
    copy(ship = newShip, previousStates = this :: previousStates)
  }

  def moveSoutheast(): GameState = {
    val newShip = ship.moveSoutheast()
    copy(ship = newShip, previousStates = this :: previousStates)
  }

  def moveSouthwest(): GameState = {
    val newShip = ship.moveSouthwest()
    copy(ship = newShip, previousStates = this :: previousStates)
  }

  def shoot(shootSound: AudioSample): GameState = {
    val baseBullet = Bullet(Point(ship.position.x + 40, ship.position.y))
    val bullets = if (meteorDestroyedCount >= 5) {

      List(baseBullet, Bullet(Point(ship.position.x + 40, ship.position.y + 100)))
    } else {
      List(baseBullet)
    }

    shootSound.trigger()
    copy(bullets = bullets ++ this.bullets)
  }
  def updateBossBullets(): GameState = {
    val updatedBossBullets = boss match {
      case Some(b) if b.defeated => List()
      case Some(b) if b.canFire() && frameCount % 60 == 0 =>
        val newBullets = List(
          BossBullet(Point(b.position.x, b.position.y + 0)),
          BossBullet(Point(b.position.x + 100, b.position.y + 350)),
          BossBullet(Point(b.position.x, b.position.y + 250))
        )
        newBullets ++ bossBullets.map(_.move()).filter(_.position.x > 0)
      case _ =>
        bossBullets.map(_.move()).filter(_.position.x > 0) // Continue moving existing bullets
    }

    val (collidingBullets, nonCollidingBullets) = updatedBossBullets.partition { bullet =>
      val shipCenter = Point(ship.position.x + 5 * 12, ship.position.y + 5 * 12)
      val shipRadius = 70

      val bulletDistance = math.sqrt(
        math.pow(bullet.position.x - shipCenter.x, 2) +
          math.pow(bullet.position.y - shipCenter.y, 2)
      )

      bulletDistance < shipRadius
    }

    val shipHit = collidingBullets.nonEmpty
    val newHealth = if (shipHit && health > 0 && shield < 1) health - 1 else health
    val newShield = if (shipHit && shield > 0) shield - 1 else shield

    copy(bossBullets = nonCollidingBullets, health = newHealth, shield = newShield)
  }



  def updateBullets(width: Float,shotSound: AudioPlayer): GameState = {
    val movedBullets = bullets.map(_.move()).filter(bullet => bullet.position.x < width)
    val updatedExplosions = explosions.map(_.nextStage()).filter(_.stage < 8)

    val (collidedBullets, remainingBullets) = movedBullets.partition { bullet =>
      checkCollision(List(bullet), meteors)
    }

    val newMeteors = meteors.filterNot { meteor =>
      checkCollision(collidedBullets, List(meteor))
    }

    val meteorsDestroyed = meteors.size - newMeteors.size

    val bossAfterDamage = boss.map { b =>
      val (bossHitBullets, nonBossBullets) = remainingBullets.partition(bullet => b.isHit(bullet.position))

      if (bossHitBullets.nonEmpty) {
        shotSound.rewind()
        shotSound.play()
      }

      val damagedBoss = bossHitBullets.foldLeft(b)((currentBoss, _) => currentBoss.takeDamage(1))
      (damagedBoss, nonBossBullets)
    }

    val (updatedBoss, finalBullets) = bossAfterDamage match {
      case Some((damagedBoss, nonBossBullets)) =>
        (Some(damagedBoss), nonBossBullets)
      case None =>
        (None, remainingBullets)
    }

    val newExplosions = collidedBullets.map(bullet => Explosion(bullet.position, 0))

    copy(
      bullets = finalBullets,
      meteors = newMeteors,
      explosions = newExplosions ++ updatedExplosions,
      boss = updatedBoss,
      meteorDestroyedCount = meteorDestroyedCount + meteorsDestroyed // Update the count
    )
  }




  def updateExplosions(): GameState = {
      val updatedExplosions = explosions.map(_.nextStage()).filter(_.stage < 8)
      copy(explosions = updatedExplosions)
    }

    def undo(): GameState = previousStates match {
      case Nil => this
      case prevState :: rest => prevState.copy(previousStates = rest)
    }

    def updateFrame(): GameState = {
      copy(frameCount = frameCount + 1)
    }
  }


  class SpaceInvaderGame extends PApplet {

    var gameState: GameState = _
    var minim: Minim = _
    var shootSound: AudioSample = _
    var explosionSound: AudioSample = _
    var backgroundMusic: AudioPlayer = _
    var shotSound: AudioPlayer = _
    var overSound: AudioPlayer = _
    override def settings(): Unit = {
      fullScreen()
    }

    override def setup(): Unit = {
      textSize(12)
      minim = new Minim(this)

      shootSound = minim.loadSample("sounds/shoot.wav")
      explosionSound = minim.loadSample("sounds/explosion.wav")
      backgroundMusic = minim.loadFile("sounds/song.mp3")
      shotSound = minim.loadFile("sounds/shot.mp3")
      overSound = minim.loadFile("sounds/over.wav")
      backgroundMusic.loop()

      shootSound.setGain(-36)       // Lower the volume of the shoot sound
      explosionSound.setGain(-36)   // Lower the volume of the explosion sound
      backgroundMusic.setGain(-36)  // Lower the background music volume more for balance
      shotSound.setGain(-18)        // Lower the volume of the shot sound
      overSound.setGain(-36)        // Lower the game-over sound volume

      val shipHeight = textAscent() + textDescent()
      val random = new scala.util.Random
      val meteorRadius = 100
      val minDistance = meteorRadius * 2

      var meteors = List[Meteor]()

      while (meteors.length < 55) {
        val randomX = width + random.nextInt(40000)
        val randomY = random.nextInt(height) + 150

        val jitterX = random.nextInt(500) - 250
        val jitterY = random.nextInt(100) - 250
        val newX = randomX + jitterX
        val newY = randomY + jitterY

        val newMeteorCenterX = newX + 10 * 12
        val newMeteorCenterY = newY + 7 * 12

        val isOverlapping = meteors.exists { meteor =>
          val existingCenterX = meteor.position.x + 10 * 12
          val existingCenterY = meteor.position.y + 7 * 12

          val distanceSquared = math.pow(newMeteorCenterX - existingCenterX, 2) +
            math.pow(newMeteorCenterY - existingCenterY, 2)

          distanceSquared < math.pow(minDistance, 2)
        }

        if (!isOverlapping) {
          meteors = Meteor(Point(newX, newY)) :: meteors
        }
      }

      gameState = GameState(
        Ship(Point(0, height - shipHeight * 20)),
        meteors = meteors
      )
    }




    override def draw(): Unit = {
      background(0)
      drawBackground()

      if (gameState.health <= 0) {
        drawGameOver("GAME OVER")
      } else if (gameState.boss.exists(_.defeated) && gameState.boss.get.position.y >= 1600) {
        drawGameOver("YOU WON!")
      } else {
        if (gameState.hit) {
          if (gameState.shield > 0) {
            gameState = gameState.copy(shield = max(gameState.shield - 1, 0))
            gameState.hit = false
          } else {
            gameState = gameState.copy(health = max(gameState.health - 1, 0))
            gameState.hit = false
          }
        }

        gameState = gameState.updateMeteor(width, height, gameState.health, gameState.hit, explosionSound)
        gameState = gameState.updateBullets(width.toFloat, shotSound)
        gameState = gameState.updateBossBullets()
        gameState = gameState.updateBeamDamage()
        gameState = gameState.updateExplosions()
        gameState = gameState.updateFrame()
        gameState = gameState.spawnBossIfNeeded()

        gameState.boss.foreach { boss =>
          val currentTime = gameState.frameCount / 60.0f
          val updatedBoss = boss.move(gameState.ship.position.y, currentTime, height.toFloat)
          updatedBoss.draw(this)
          drawBossHealthBar(updatedBoss.health, 100)
          gameState = gameState.copy(boss = Some(updatedBoss))
        }

        if (gameState.beamActive) {
          gameState.boss.foreach { boss =>
            val beam = Beam(Point(boss.position.x, boss.position.y + 200))
            beam.draw(this)
          }
        }

        drawRadio()
        drawMeteor()
        drawShip()
        drawBullets()
        drawBossBullets()
        drawExplosions()
        drawHealthBar(gameState.health, gameState.maxHealth, gameState.shield, gameState.maxShield)
        drawMeteorCounter()
      }
    }

    def drawMeteorCounter(): Unit = {
      fill(0,255,0)
      textSize(24)
      text(s"Meteors Destroyed: ${gameState.meteorDestroyedCount}", 20, 40)
      text("Hint: Destroy meteors to get double cannon upgrade", 20, 60)
    }




    def drawGameOver(message:String): Unit = {
      fill(255, 0, 0)
      textSize(100)
      textAlign(width/2 - 250, height/2 +50)
      text(message, width / 2 - 220 , height / 2 - 50)

      textSize(50)
      text("Hold U to reverse the gamestate..", width / 2 - 400, height / 2 + 50)
    }

    def drawRadio(): Unit = {
      val boxX = 230
      val boxY = height - 100
      val radioText: String =
        """Incomming transmission:
Finally! I got a signal.. (Someone is still out there?..muffled). Ranger, we have no time,
get ready! There is a meteor shower incoming! Use the trusters to evade them:
(W,A,S,D). Cannon should be working, fire it(" ")!!! Also the mother shi&^&sskklDSGH2."""

      if (gameState.radioCharCount < radioText.length) {
        gameState = gameState.copy(radioCharCount = gameState.radioCharCount + 1)
      }

      val displayedMessage = radioText.substring(0, gameState.radioCharCount)

      noFill()
      stroke(0)
      rect(boxX, boxY, width, height)

      fill(0, 255, 0)
      textSize(16)

      val radioLines = List(
        "_______________________________________________________________________",
        "|                                                                                                                                            |",
        "|                                                                                                                                            |",
        "|                                                                                                                                            |",
        "|______________________________________________________________________|"
      )

      for ((line, index) <- radioLines.zipWithIndex) {
        text(line, boxX + 10, boxY + 10 + index * 20)
      }

      text(displayedMessage, boxX + 20, boxY - 70 + radioLines.length * 20)
    }

    def drawBossHealthBar(bossHealth: Int, maxHealth: Int): Unit = {
      val totalHealthPoints = 10
      val healthPoints = (bossHealth.toFloat / maxHealth * totalHealthPoints).round.toInt
      val healthBar = "[" + "+" * healthPoints + " " * (totalHealthPoints - healthPoints) + "]"

      fill(255, 0, 0)
      textSize(30)
      text(healthBar, width / 2 - 50, 50)
    }
    def drawHealthBar(health: Int, maxHealth: Int, shield: Int, maxShield: Int): Unit = {
      val totalShieldPoints = 10
      val totalHealthPoints = 10
      val healthPoints = (health.toFloat / maxHealth * totalHealthPoints).round.toInt
      val shieldPoints = (shield.toFloat / maxShield * totalShieldPoints).round.toInt
      val healthBar = "[" + "+" * healthPoints + "" * (totalHealthPoints - healthPoints) + "]"
      val shieldBar = "[" + "=" * shieldPoints + "-" * (totalShieldPoints - shieldPoints) + "]"
      fill(255, 0, 0)
      textSize(30)
      text(healthBar, 10, height - 30)
      fill(0, 0, 255)
      text(shieldBar, 10, height - 60)
    }

    def drawBackground(): Unit = {
      fill(128, 128, 128, 50)
      val backgroundArt =
        """ ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣀⡀⠀⢀⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠄⠂⠲⡟⠉⠂⠂⠀⠀⠁⠀⠀⠀⠀⠀⠁⠀⠐⠠⠄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⠔⠈⠀⢀⠀⡄⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠂⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠐⠁⠀⠠⠀⠊⠀⢸⢩⣆⠠⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠂⢄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⡠⠊⠀⠀⠚⡌⠁⠀⠀⢀⡀⢿⠢⡱⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠁⠢⡀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⢠⣾⢀⣀⠀⠀⠀⠁⠀⢀⠴⠉⠐⠏⠣⠎⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⢄⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⢀⠴⠋⢀⠺⠁⠘⠈⠀⠀⠠⠂⠀⠃⡄⣀⡤⠄⠀⠀⢀⠤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠡⡀⠀⠀⠀⠀⠀
        ⠀⠀⠀⢀⠖⠁⢀⡀⢃⠱⡀⠀⡄⠈⠀⠀⡼⠘⠈⡝⠂⠠⡴⡶⠌⠦⠨⣀⠁⠈⠀⠂⠀⢄⠀⠀⠀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⡄⠀⠀⠀⠀
        ⠀⠀⠀⢺⠀⠀⠈⠠⠈⠰⢊⠀⠀⠤⠄⠤⠂⠸⡌⠌⠂⠀⠀⠀⠀⠀⠨⠆⢈⠇⣀⠀⠀⣀⠠⡐⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⡄⠀⠀⠀
        ⠀⠀⠌⠌⠀⠀⠀⠀⠑⠀⢈⠀⢀⡠⢐⡄⠔⠠⠆⠛⢲⡐⠋⠀⡀⠀⠀⠀⠀⠀⠀⠁⡀⠀⠁⠌⠰⠄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⡀⠀⠀
        ⠀⡘⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⡀⡅⠀⠀⠀⠀⠀⠀⠀⠰⣀⠀⠀⠀⠀⠀⠀⠋⠢⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠁⠀⠀
        ⠀⠄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡠⠄⠀⠈⠀⠀⠀⠀⠈⠐⠡⠂⠀⠘⡀⠀⠀⠀⠀⠀⠀⠁⠢⡖⡀⠀⠀⠀⠀⠀⠀⠀⠀⠈⡀⠆
        ⠀⢰⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠑⠠⠀⠀⠀⠀⠀⠀⠀⠒⠸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠃⠀
        ⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⠊⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠢⡀⠀⠀⠀⠀⠀⠘⡢⡀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠀
        ⠀⠠⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢈⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⠀⢸⠀
        ⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠂⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀
        ⠀⠈⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢃⠀⠀⠀⠀⠀⠄⠂⠈⠁⢄⠠⡄⠀⠀⠀⠀⠀⠀⠀⠂⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠇⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠀⠔⠈⠁⠀⠀⠀⠀⠀⠀⠐⠛⢄⠀⠀⠀⠀⢠⠂⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠐⠀⠀
        ⠀⠸⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠐⢂⠒⡈⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⡸⡁⠀⠀⠀⠀⠀⠀⠄⠀⠀
        ⠀⠀⠑⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢀⠅⠀⠀⠀⠀⠀⠀⠀⠀⡠⠪⠤⠊⠀⠀⠀⠀⠀⠜⠀⠀⠀
        ⠀⠀⠀⠣⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡔⠀⠔⠀⠀⠀⠀⠀⠀⠀⡐⠀⠀⠀⠀
        ⠀⠀⠀⠀⠐⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⢀⠌⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠈⢄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠊⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠑⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠄⠁⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠑⠄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠔⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠀⢄⠀⠀⠀⠀⠀⡀⠤⠀⠀⠐⠀⠀⠐⠂⠀⠐⠀⠀⠄⡀⠀⠀⠀⠀⠀⠀⠀⡀⠄⠊⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⠰⢒⠀⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⠅⡀⢀⡀⠤⠂⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠒⠠⠤⠀⠀⢀⠀⡀⠀⠀⠠⠄⠐⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀""".stripMargin
      val desiredWidth = width / 3
      textSize(10)
      val originalTextWidth = textWidth(backgroundArt)
      val scaleFactor = desiredWidth / originalTextWidth.toFloat
      val newTextSize = 10 * scaleFactor
      textSize(newTextSize)
      val x = (width - textWidth(backgroundArt)) / 2
      val y = (height - textAscent() - textDescent()) / 10
      text(backgroundArt, x, y)
      for (dx <- -1 to 1; dy <- -1 to 1) {
        text(backgroundArt, x + dx, y + dy)
      }
    }

    def drawShip(): Unit = {
      fill(255)
      textSize(12)
      val xOffset = 2 * sin(gameState.frameCount * 0.05).toFloat
      val yOffset = 3 * sin(gameState.frameCount * 0.03).toFloat
      val newPosition = Point(gameState.ship.position.x + xOffset, gameState.ship.position.y + yOffset)
      val asciiArt =
        """         [=====>
         [  (    _____
          \__\,-'//   --._
          [_/~||,-----.\@_\\___
          [_) |||()()()   ~[|||>
          [_\_||-----'   //
          /  /-.\\___,--'==(-
        [  (
        [=====>""".stripMargin

      text(asciiArt, newPosition.x, newPosition.y)
    }

    def drawBullets(): Unit = {
      fill(255, 0, 0)
      gameState.bullets.foreach { bullet =>
        ellipse(bullet.position.x, bullet.position.y, 15, 8)
      }
    }

    def drawBossBullets(): Unit = {
      gameState.bossBullets.foreach { bullet =>
        bullet.draw(this)
      }
    }

    def drawMeteor(): Unit = {
      gameState.meteors.foreach { meteor =>
        meteor.draw(this)

      }
    }

    def drawExplosions(): Unit = {
      gameState.explosions.foreach { explosion =>
        explosion.draw(this)
      }
    }

    var leftPressed = false
    var rightPressed = false
    var upPressed = false
    var downPressed = false
    var uPressed = false

    override def keyPressed(): Unit = {

      gameState = key match {
        case 'a' | 'A' =>
          gameState.moveLeft()
        case 'd' | 'D' => gameState.moveRight()
        case 'w' | 'W' => gameState.moveUp()
        case 's' | 'S' => gameState.moveDown()
        case ' ' => gameState.shoot(shootSound)
        case 'u' | 'U' => gameState.undo()
        case _ => gameState
      }

    }

    override def keyReleased(): Unit = {
      key match {
        case 'a' | 'A' => leftPressed = false
        case 'd' | 'D' => rightPressed = false
        case 'w' | 'W' => upPressed = false
        case 's' | 'S' => downPressed = false
        case 'u' | 'U' => uPressed = false
        case _ =>
      }
    }
  }

  object SpaceInvaderGame {
    def main(args: Array[String]): Unit = {
      PApplet.main("game.SpaceInvaderGame")
    }

}
