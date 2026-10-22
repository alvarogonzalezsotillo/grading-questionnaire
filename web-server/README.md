# web-server

Web application that replaces the desktop GUI of the **blob-detect** project.

* The browser shows the camera of the device with the maximum resolution it can give.
* The browser sends the frames to the server.
* The server looks for a valid questionnaire in every frame (using the
  `cellsLocationStep` of blob-detect). When it finds one, the image is stored in the server
  and shown in the browser as a thumbnail under the video.
* The thumbnails can be enlarged (click) and all of them can be saved to the disk of the user
  (button *Guardar todas*).

## How to run it

```bash
./gradlew :web-server:run
```

Then open <http://localhost:8080> in the browser and press *Activar cámara*.

Options:

```bash
./gradlew :web-server:run --args="--port 9000 --captures-dir /tmp/captures --detector blob-detect"
```

| option | description | default |
| --- | --- | --- |
| `--port` | the port the server listens to | `8080` |
| `--host` | the address the server listens to | `0.0.0.0` |
| `--captures-dir` | where the detected questionnaires are stored | `web-server/captures` |
| `--detector` | the detector to use | the first one available |
| `--min-interval-ms` | the minimum time between two accepted questionnaires | `2000` |
| `--duplicate-threshold` | how different two images must be to be considered different questionnaires | `5.0` |
| `--list-detectors` | lists the available detectors | |

A distribution (a start script with all the dependencies) can be built with:

```bash
./gradlew :web-server:installDist
./web-server/build/install/web-server/bin/web-server
```

## The API

| method | url | description |
| --- | --- | --- |
| `POST` | `/api/detect` | the body is the image (jpeg or png). It answers with a json with the information of the questionnaire when it has been found |
| `GET` | `/api/captures` | the list of the questionnaires detected so far |
| `DELETE` | `/api/captures` | removes every capture |
| `GET` | `/api/captures/{id}` | the image of a capture |
| `DELETE` | `/api/captures/{id}` | removes a capture |
| `GET` | `/api/status` | information about the server |
| `GET` | `/` | the web application |

Example:

```bash
curl -X POST --data-binary @questionnaire.jpg -H "Content-Type: image/jpeg" http://localhost:8080/api/detect
```

```json
{
  "detected": true,
  "duplicate": false,
  "capture": {
    "id": "20261022-170051-434-001",
    "fileName": "20261022-170051-434-001.jpg",
    "url": "/api/captures/20261022-170051-434-001",
    "timestamp": 1792681251434,
    "width": 1280,
    "height": 1024,
    "qr": "AgIYIr1rdeKm",
    "version": 2,
    "questions": 24,
    "cells": 24
  }
}
```

## The detection (SPI)

The detection is an SPI: `webserver.spi.QuestionnaireDetector`.

```scala
trait QuestionnaireDetector {
  def name: String
  def detect(image: Mat): Option[DetectedQuestionnaire]
}
```

The implementations are discovered with `java.util.ServiceLoader`, so a new one can be added
just by putting a jar with the implementation and its
`META-INF/services/webserver.spi.QuestionnaireDetector` file in the classpath.

The implementation that comes with the project is
`webserver.detector.BlobDetectQuestionnaireDetector`: it runs the processing chain of
blob-detect until the *Localización de celdas (basada en columnas)* step
(`ProcessingStep.cellsLocationStep`). That step is only able to locate the answer cells when
the QR code has been decoded and the measures of the answer matrix are known, so a successful
location of the cells means that a valid questionnaire has been found.

```bash
./gradlew :web-server:run --args="--list-detectors"
```

## Repeated questionnaires

The browser sends a frame every 500 ms, so the same questionnaire is detected many times in a
row. Those repetitions are not stored:

* A questionnaire with a different QR code is always stored (it is another questionnaire).
* When the image is almost the same as the last stored one, it is considered a repetition.
  The reference image is updated, so a questionnaire that is moved slowly is not stored again.
* When the scene has changed but less than `--min-interval-ms` has passed, the image is
  discarded too (it will be detected again a few seconds later, once the interval has passed).

The *difference* between two images is measured on a 32x24 grayscale version of the whole
frame, and it is compared with `--duplicate-threshold`.

## Notes about the camera

The browsers only give access to the camera in a *secure context*: `http://localhost` or
`https`. When the server is used from another device (a tablet, for instance) it has to be
published over https (for example with a reverse proxy) or the camera will not work.

The images are normalized to 1280 pixels wide by the blob-detect pipeline, so the frames are
sent with a smaller resolution (1600 pixels wide by default) to avoid saturating the network.
It can be changed with the *Envío* selector of the web application.
