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

// ====================
// MCP Watch (polling)
// ====================

(function() {
    let mcpTimerId = null;
    let watching = false;

    window.startMcpWatch = function(intervalMs) {
        if (mcpTimerId !== null) return;
        watching = true;
        mcpTimerId = setInterval(async function() {
            try {
                const resp = await fetch('/api/mcp/frame');
                if (resp.status !== 200) return;
                const buf = await resp.arrayBuffer();
                const byteArray = new Uint8Array(buf);
                window.drawScreen(byteArray);
            } catch (_) {}
        }, intervalMs || 50);
        updateMcpButton();
    };

    window.stopMcpWatch = function() {
        if (mcpTimerId !== null) {
            clearInterval(mcpTimerId);
            mcpTimerId = null;
        }
        watching = false;
        updateMcpButton();
    };

    window.toggleMcpWatch = function() {
        if (watching) {
            window.stopMcpWatch();
        } else {
            window.startMcpWatch(50);
        }
    };

    function updateMcpButton() {
        const btn = document.getElementById('mcp-watch-btn');
        if (!btn) return;
        if (watching) {
            btn.textContent = 'Stop MCP Watch';
            btn.style.background = '#e74c3c';
        } else {
            btn.textContent = 'Watch MCP';
            btn.style.background = '#9b59b6';
        }
    }

    // Insert MCP Watch button after DOM is ready
    function insertMcpButton() {
        const buttons = document.querySelector('div[style*="display: flex; gap: 10px"]');
        if (!buttons) { setTimeout(insertMcpButton, 200); return; }
        if (document.getElementById('mcp-watch-btn')) return;
        const btn = document.createElement('button');
        btn.id = 'mcp-watch-btn';
        btn.textContent = 'Watch MCP';
        btn.style.cssText = 'padding: 10px 20px; font-size: 14px; cursor: pointer; background: #9b59b6; color: white; border: none; border-radius: 5px;';
        btn.onclick = window.toggleMcpWatch;
        buttons.appendChild(btn);
    }
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', insertMcpButton);
    } else {
        insertMcpButton();
    }
})();

console.log('gbfs app.js loaded');
