// Draw the Game Boy screen to canvas
window.drawScreen = function(byteArray) {
    const canvas = document.getElementById('emulator-screen');
    if (!canvas) {
        console.warn('Canvas element not found');
        return;
    }

    const ctx = canvas.getContext('2d');
    if (!ctx) {
        console.warn('Could not get 2D context');
        return;
    }

    const imageData = ctx.createImageData(160, 144);

    // GameBoy Grayscale Palette (classic green tint)
    const colors = [
        [224, 248, 208, 255], // 0 - Lightest (White/Light Green)
        [136, 192, 112, 255], // 1 - Light Gray
        [52, 104, 86, 255],   // 2 - Dark Gray
        [8, 24, 32, 255]      // 3 - Darkest (Black/Dark Green)
    ];

    for (let i = 0; i < byteArray.length && i < 160 * 144; i++) {
        const colorIndex = byteArray[i] & 0x03; // Ensure valid index (0-3)
        const color = colors[colorIndex];
        imageData.data[i * 4 + 0] = color[0]; // R
        imageData.data[i * 4 + 1] = color[1]; // G
        imageData.data[i * 4 + 2] = color[2]; // B
        imageData.data[i * 4 + 3] = color[3]; // A
    }

    ctx.putImageData(imageData, 0, 0);
};

// Read ROM file and return as Base64 string
window.readRomFile = async function() {
    const fileInput = document.getElementById('rom-input');
    if (!fileInput) {
        throw new Error('File input not found');
    }

    const file = fileInput.files[0];
    if (!file) {
        throw new Error('No file selected');
    }

    console.log('Reading file:', file.name, 'Size:', file.size);

    const arrayBuffer = await file.arrayBuffer();
    const byteArray = new Uint8Array(arrayBuffer);

    // Convert to Base64
    let binary = '';
    for (let i = 0; i < byteArray.length; i++) {
        binary += String.fromCharCode(byteArray[i]);
    }
    const base64Data = btoa(binary);

    console.log('ROM read successfully, base64 length:', base64Data.length);
    return base64Data;
};

// Check if a file is selected
window.hasFileSelected = function() {
    const fileInput = document.getElementById('rom-input');
    return fileInput && fileInput.files && fileInput.files.length > 0;
};

// Clear the canvas to default Game Boy green
window.clearScreen = function() {
    const canvas = document.getElementById('emulator-screen');
    if (!canvas) return;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    ctx.fillStyle = '#9bbc0f';
    ctx.fillRect(0, 0, 160, 144);
};

// Request animation frame wrapper for smooth frame timing
window.requestAnimationFrameAsync = function() {
    return new Promise(resolve => {
        requestAnimationFrame(() => resolve());
    });
};

// ====================
// Audio
// ====================

(function() {
    let audioCtx = null;
    let audioQueue = [];
    let isPlaying = false;
    const SAMPLE_RATE = 44100;
    const BUFFER_SIZE = 2048; // samples per channel per chunk

    window.initAudio = function() {
        if (audioCtx) return;
        audioCtx = new (window.AudioContext || window.webkitAudioContext)({ sampleRate: SAMPLE_RATE });
        audioQueue = [];
        isPlaying = false;
        console.log('Audio initialized, sample rate:', audioCtx.sampleRate);
    };

    function playNextBuffer() {
        if (!audioCtx || audioQueue.length === 0) {
            isPlaying = false;
            return;
        }
        isPlaying = true;

        const samples = audioQueue.shift();
        const numSamples = Math.floor(samples.length / 2); // L/R interleaved
        const buffer = audioCtx.createBuffer(2, numSamples, SAMPLE_RATE);
        const left = buffer.getChannelData(0);
        const right = buffer.getChannelData(1);

        for (let i = 0; i < numSamples; i++) {
            left[i] = samples[i * 2];
            right[i] = samples[i * 2 + 1];
        }

        const source = audioCtx.createBufferSource();
        source.buffer = buffer;
        source.connect(audioCtx.destination);
        source.onended = playNextBuffer;
        source.start();
    }

    window.queueAudioSamples = function(float32Array) {
        if (!audioCtx) return;
        // Keep queue bounded to avoid lag buildup
        if (audioQueue.length > 4) {
            audioQueue.splice(0, audioQueue.length - 2);
        }
        audioQueue.push(float32Array);
        if (!isPlaying) {
            playNextBuffer();
        }
    };
})();

console.log('gbfs app.js loaded');
