
Download the Adafruit boards and select **Adafruit Gemma M0 (SAMD21)**

TODO fill this out from the adafruit-board-hack.txt

Additions to Adafruit boards:

**`samd/1.7.13/variants/neotrinkey_m0/variant.h`**

```C
// Fake SPI Interface just so we can compile
#define SPI_INTERFACES_COUNT 1

#define PIN_SPI_MISO         PIN_A0
#define PIN_SPI_MOSI         PIN_A0
#define PIN_SPI_SCK          PIN_A0
#define PERIPH_SPI           sercom0
#define PAD_SPI_TX           SPI_PAD_0_SCK_1
#define PAD_SPI_RX           SERCOM_RX_PAD_0

static const uint8_t SS	  = PIN_A0;
static const uint8_t MOSI = PIN_SPI_MOSI;
static const uint8_t MISO = PIN_SPI_MISO;
static const uint8_t SCK  = PIN_SPI_SCK;
#define WIRE_INTERFACES_COUNT 1

#define PIN_WIRE_SDA         (0u)
#define PIN_WIRE_SCL         (2u)
#define PERIPH_WIRE          sercom0
//#define WIRE_IT_HANDLER    // hack! we call the i2c handler from within the serial handler!

static const uint8_t SDA = PIN_WIRE_SDA;
static const uint8_t SCL = PIN_WIRE_SCL;

#define PIN_LED_13           (13u)  
#define LED_BUILTIN          PIN_LED_13

  
/*
 * Serial interfaces
 */

// Serial1 (sercom 0)
#define PIN_SERIAL1_RX       (2ul) // PA05
#define PAD_SERIAL1_RX       (SERCOM_RX_PAD_1)
#define PIN_SERIAL1_TX       (0ul) // PA04
#define PAD_SERIAL1_TX       (UART_TX_PAD_0)

extern Uart Serial1;
```


**`samd/1.7.13/variants/neotrinkey_m0/variant.cpp`**

```C
Uart Serial1( &sercom0, PIN_SERIAL1_RX, PIN_SERIAL1_TX, PAD_SERIAL1_RX, PAD_SERIAL1_TX ) ;
```