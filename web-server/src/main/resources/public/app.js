'use strict';

/*
 * Web client of the questionnaire detector.
 *
 *  - It shows the camera of the device with the maximum resolution it can give.
 *  - It sends the frames to the server, that looks for valid questionnaires.
 *  - When the server finds a questionnaire it is stored in the server and shown here as a
 *    thumbnail under the video. The thumbnails can be enlarged and saved to the disk.
 */


function seal(o){
   return  Object.seal(o);
}

const state = {
    stream: null,
    track: null,
    timer: null,
    busy: false,
    running: false,
    captures: [],
    intervalMs: 500,
    captureWidth: 1600,
    sent: 0
};

const elements = {};

function byId(id) {
    return document.getElementById(id);
}

function setStatus(text, kind) {
    elements.status.textContent = text;
    elements.status.className = 'status' + (kind ? ' ' + kind : '');
}

/* ------------------------------------------------------------------ camera */

async function startCamera() {
    if (!window.isSecureContext) {
        setStatus('La cámara sólo está disponible en localhost o sobre https', 'error');
        return;
    }
    if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
        setStatus('Este navegador no permite usar la cámara', 'error');
        return;
    }

    setStatus('Pidiendo la cámara…');
    try {
        const stream = await navigator.mediaDevices.getUserMedia({
            audio: false,
            video: {
                width: {ideal: 4096},
                height: {ideal: 2160},
                frameRate: {ideal: 60}
            }
        });

        state.stream = stream;
        state.track = stream.getVideoTracks()[0];

        elements.video.srcObject = stream;
        await elements.video.play();

        await maximizeResolution();
        showVideoInfo();

        elements.videoPlaceholder.classList.add('hidden');
        elements.toggleCamera.textContent = 'Detener cámara';
        elements.toggleCamera.classList.remove('primary');
        setStatus('Cámara activa', 'ok');

        state.running = true;
        startCaptureLoop();
    } catch (error) {
        setStatus('No se ha podido abrir la cámara: ' + error.message, 'error');
    }
}

function stopCamera() {
    state.running = false;
    stopCaptureLoop();
    if (state.stream) {
        state.stream.getTracks().forEach(track => track.stop());
    }
    state.stream = null;
    state.track = null;
    elements.video.srcObject = null;
    elements.videoPlaceholder.classList.remove('hidden');
    elements.toggleCamera.textContent = 'Activar cámara';
    elements.toggleCamera.classList.add('primary');
    setStatus('Cámara apagada');
}

/**
 * Asks the camera for the biggest resolution it is able to give. Most of the browsers give
 * the first resolution that satisfies the constraints, so the capabilities of the track are
 * inspected and the maximum one is requested explicitly.
 */
async function maximizeResolution() {
    const track = state.track;
    if (!track || !track.getCapabilities) {
        return;
    }

    const capabilities = track.getCapabilities();
    const maxWidth = (capabilities.width && capabilities.width.max) || 0;
    const maxHeight = (capabilities.height && capabilities.height.max) || 0;
    if (!maxWidth) {
        return;
    }

    const candidates = [];
    if (maxHeight) {
        candidates.push({width: {ideal: maxWidth}, height: {ideal: maxHeight}});
    }
    candidates.push({width: {ideal: maxWidth}});
    if (maxHeight) {
        candidates.push({height: {ideal: maxHeight}});
    }

    for (const constraints of candidates) {
        try {
            await track.applyConstraints(constraints);
            const settings = track.getSettings ? track.getSettings() : {};
            if ((settings.width || 0) >= maxWidth * 0.9) {
                break;
            }
        } catch (error) {
            // this combination of constraints is not supported, the next one is tried
        }
    }
}

function showVideoInfo() {
    const track = state.track;
    if (!track || !track.getSettings) {
        elements.videoInfo.textContent = '';
        return;
    }
    const settings = track.getSettings();
    elements.videoInfo.textContent =
        `${settings.width || '?'}x${settings.height || '?'} @ ${settings.frameRate || '?'} fps` +
        ` | enviadas: ${state.sent}`;
}

/* ---------------------------------------------------------------- capturing */

function startCaptureLoop() {
    stopCaptureLoop();
    const tick = async () => {
        if (!state.running) {
            return;
        }
        await captureAndSend();
        if (state.running) {
            state.timer = setTimeout(tick, state.intervalMs);
        }
    };
    state.timer = setTimeout(tick, 0);
}

function stopCaptureLoop() {
    if (state.timer) {
        clearTimeout(state.timer);
        state.timer = null;
    }
}

function currentFrameBlob() {
    const video = elements.video;
    if (!video.videoWidth || !video.videoHeight) {
        return Promise.resolve(null);
    }

    let width = video.videoWidth;
    if (state.captureWidth > 0) {
        width = Math.min(state.captureWidth, video.videoWidth);
    }
    const height = Math.round(width * video.videoHeight / video.videoWidth);

    const canvas = elements.canvas;
    canvas.width = width;
    canvas.height = height;

    const context = canvas.getContext('2d');
    context.drawImage(video, 0, 0, width, height);

    return new Promise(resolve => canvas.toBlob(resolve, 'image/jpeg', 0.85));
}

async function captureAndSend() {
    if (state.busy) {
        return;
    }
    state.busy = true;
    try {
        const blob = await currentFrameBlob();
        if (!blob) {
            return;
        }
        state.sent++;
        showVideoInfo();

        const response = await fetch('/api/detect', {
            method: 'POST',
            body: blob,
            headers: {'Content-Type': 'image/jpeg'}
        });
        if (!response.ok) {
            throw new Error('the server answered ' + response.status);
        }
        const result = await response.json();
        if (result.detected && result.capture) {
            addCapture(result.capture);
        }
    } catch (error) {
        setStatus('Error enviando la imagen: ' + error.message, 'error');
    } finally {
        state.busy = false;
    }
}

/* ---------------------------------------------------------------- captures */

function addCapture(capture) {
    if (state.captures.some(c => c.id === capture.id)) {
        return;
    }
    state.captures.unshift(capture);
    renderCaptures();
    setStatus('Cuestionario detectado: ' + capture.fileName, 'ok');
}

function renderCaptures() {
    const list = elements.thumbnailList;
    list.innerHTML = '';

    if (!state.captures.length) {
        const empty = document.createElement('p');
        empty.className = 'empty';
        empty.textContent = 'Todavía no se ha detectado ningún cuestionario.';
        list.appendChild(empty);
    }

    state.captures.forEach(capture => {
        const item = document.createElement('div');
        item.className = 'thumbnail';

        const image = document.createElement('img');
        image.src = capture.url;
        image.alt = capture.fileName;
        image.addEventListener('click', () => openLightbox(capture));

        const caption = document.createElement('div');
        caption.className = 'caption';
        caption.textContent = describeCapture(capture);

        const remove = document.createElement('button');
        remove.className = 'remove';
        remove.innerHTML = '&times;';
        remove.title = 'Eliminar';
        remove.addEventListener('click', event => {
            event.stopPropagation();
            deleteCapture(capture);
        });

        item.appendChild(image);
        item.appendChild(caption);
        item.appendChild(remove);
        list.appendChild(item);
    });

    elements.captureCount.textContent = state.captures.length;
}

function describeCapture(capture) {
    const time = new Date(capture.timestamp).toLocaleTimeString();
    const parts = [time];
    if (capture.questions) {
        parts.push(capture.questions + ' preguntas');
    }
    if (capture.cells) {
        parts.push(capture.cells + ' celdas');
    }
    if (capture.version !== null && capture.version !== undefined) {
        parts.push('v' + capture.version);
    }
    return parts.join(' · ');
}

async function deleteCapture(capture) {
    try {
        await fetch('/api/captures/' + encodeURIComponent(capture.id), {method: 'DELETE'});
    } catch (error) {
        // the capture is removed from the list anyway
    }
    state.captures = state.captures.filter(c => c.id !== capture.id);
    renderCaptures();
}

async function clearCaptures() {
    if (!state.captures.length) {
        return;
    }
    if (!window.confirm('¿Seguro que quieres eliminar todos los cuestionarios detectados?')) {
        return;
    }
    try {
        await fetch('/api/captures', {method: 'DELETE'});
    } catch (error) {
        // ignored
    }
    state.captures = [];
    renderCaptures();
    setStatus('Lista vaciada');
}

/** Downloads every detected questionnaire to the disk of the user */
async function saveAll() {
    if (!state.captures.length) {
        setStatus('No hay cuestionarios que guardar', 'warn');
        return;
    }
    setStatus('Guardando ' + state.captures.length + ' cuestionarios…');
    for (const capture of state.captures) {
        try {
            const response = await fetch(capture.url);
            const blob = await response.blob();
            const url = URL.createObjectURL(blob);
            const anchor = document.createElement('a');
            anchor.href = url;
            anchor.download = capture.fileName || ('cuestionario-' + capture.id + '.jpg');
            document.body.appendChild(anchor);
            anchor.click();
            document.body.removeChild(anchor);
            setTimeout(() => URL.revokeObjectURL(url), 10000);
            // some browsers block consecutive downloads
            await new Promise(resolve => setTimeout(resolve, 350));
        } catch (error) {
            setStatus('Error guardando ' + capture.fileName + ': ' + error.message, 'error');
        }
    }
    setStatus('Cuestionarios guardados', 'ok');
}

/* ---------------------------------------------------------------- lightbox */

function openLightbox(capture) {
    elements.lightboxImage.src = capture.url;
    elements.lightboxImage.alt = capture.fileName;
    elements.lightboxDownload.href = capture.url;
    elements.lightboxDownload.setAttribute('download', capture.fileName || 'cuestionario.jpg');
    elements.lightboxInfo.textContent = describeCapture(capture);
    elements.lightbox.classList.remove('hidden');
}

function closeLightbox() {
    elements.lightbox.classList.add('hidden');
    elements.lightboxImage.src = '';
}

/* -------------------------------------------------------------------- init */

async function loadExistingCaptures() {
    try {
        const response = await fetch('/api/captures');
        if (!response.ok) {
            return;
        }
        const captures = await response.json();
        state.captures = captures;
        renderCaptures();
    } catch (error) {
        // the server is not reachable, it will be shown in the status
    }
}

async function refreshServerInfo() {
    try {
        const response = await fetch('/api/status');
        if (!response.ok) {
            return;
        }
        const status = await response.json();
        elements.serverInfo.textContent =
            `servidor: ${status.detector} · ${status.captures} cuestionarios · ` +
            `${status.analyzedFrames} imágenes analizadas`;
    } catch (error) {
        elements.serverInfo.textContent = 'el servidor no responde';
    }
}

function main() {
    elements.video = byId('video');
    elements.canvas = byId('capture-canvas');
    elements.status = byId('status');
    elements.serverInfo = byId('server-info');
    elements.videoPlaceholder = byId('video-placeholder');
    elements.videoInfo = byId('video-info');
    elements.toggleCamera = byId('toggle-camera');
    elements.captureNow = byId('capture-now');
    elements.saveAll = byId('save-all');
    elements.clear = byId('clear');
    elements.interval = byId('interval');
    elements.captureWidth = byId('capture-width');
    elements.thumbnailList = byId('thumbnail-list');
    elements.captureCount = byId('capture-count');
    elements.lightbox = byId('lightbox');
    elements.lightboxImage = byId('lightbox-image');
    elements.lightboxInfo = byId('lightbox-info');
    elements.lightboxDownload = byId('lightbox-download');
    elements.lightboxClose = byId('lightbox-close');

    elements.toggleCamera.addEventListener('click', () => {
        if (state.running) {
            stopCamera();
        } else {
            startCamera();
        }
    });

    elements.captureNow.addEventListener('click', () => {
        if (state.running) {
            captureAndSend();
        } else {
            setStatus('Activa la cámara para poder capturar', 'warn');
        }
    });

    elements.saveAll.addEventListener('click', saveAll);
    elements.clear.addEventListener('click', clearCaptures);

    elements.interval.addEventListener('change', event => {
        state.intervalMs = parseInt(event.target.value, 10);
    });

    elements.captureWidth.addEventListener('change', event => {
        state.captureWidth = parseInt(event.target.value, 10);
    });

    elements.lightboxClose.addEventListener('click', closeLightbox);
    elements.lightbox.addEventListener('click', event => {
        if (event.target === elements.lightbox) {
            closeLightbox();
        }
    });

    document.addEventListener('keydown', event => {
        if (event.key === 'Escape') {
            closeLightbox();
        }
        if (event.code === 'Space' && state.running) {
            event.preventDefault();
            captureAndSend();
        }
    });

    loadExistingCaptures();
    refreshServerInfo();
    setInterval(refreshServerInfo, 5000);
    setStatus('Listo. Pulsa "Activar cámara".');
}

window.addEventListener('load', main);
