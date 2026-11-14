// SPDX-License-Identifier: AGPL-3.0-or-later

const PROCESSOR_NAME = 'fluxer-deep-filter';
const RENDER_QUANTUM = 128;
const WARM_UP_FRAMES = 8;
const HEALTH_REPORT_FRAMES = 50;
const MAX_NON_FINITE_FRAMES = 3;

class Utf8Decoder {
	decode(bytes) {
		if (!bytes) return '';
		let out = '';
		for (let i = 0; i < bytes.length; ) {
			const c = bytes[i++];
			if (c < 0x80) out += String.fromCharCode(c);
			else if (c < 0xe0) out += String.fromCharCode(((c & 0x1f) << 6) | (bytes[i++] & 0x3f));
			else if (c < 0xf0)
				out += String.fromCharCode(((c & 0x0f) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i++] & 0x3f));
			else {
				const cp =
					(((c & 0x07) << 18) | ((bytes[i++] & 0x3f) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i++] & 0x3f)) -
					0x10000;
				out += String.fromCharCode(0xd800 + (cp >> 10), 0xdc00 + (cp & 0x3ff));
			}
		}
		return out;
	}
}

function createDeepFilterBindings(module) {
	let wasm = null;
	let float32Memory = null;
	let uint8Memory = null;
	let vectorLength = 0;
	const decoder = typeof TextDecoder === 'undefined' ? new Utf8Decoder() : new TextDecoder('utf-8');
	const frameOutputs = new Map();

	const getFloat32Memory = () => {
		if (float32Memory === null || float32Memory.byteLength === 0) float32Memory = new Float32Array(wasm.memory.buffer);
		return float32Memory;
	};
	const getUint8Memory = () => {
		if (uint8Memory === null || uint8Memory.byteLength === 0) uint8Memory = new Uint8Array(wasm.memory.buffer);
		return uint8Memory;
	};
	const getFloat32Slice = (ptr, len) => {
		const start = (ptr >>> 0) / 4;
		return getFloat32Memory().subarray(start, start + len);
	};
	const getUint8Slice = (ptr, len) => {
		const start = ptr >>> 0;
		return getUint8Memory().subarray(start, start + len);
	};
	const passUint8Array = (array) => {
		const ptr = wasm.__wbindgen_malloc_command_export(array.length, 1) >>> 0;
		getUint8Memory().set(array, ptr);
		vectorLength = array.length;
		return ptr;
	};
	const passFloat32Array = (array) => {
		const ptr = wasm.__wbindgen_malloc_command_export(array.length * 4, 4) >>> 0;
		getFloat32Memory().set(array, ptr / 4);
		vectorLength = array.length;
		return ptr;
	};
	const storeException = (error) => {
		const index = wasm.__externref_table_alloc_command_export();
		wasm.__wbindgen_externrefs.set(index, error);
		wasm.__wbindgen_exn_store_command_export(index);
	};

	const imports = {
		'./df_bg.js': {
			__wbg___wbindgen_throw_344f42d3211c4765(ptr, len) {
				throw new Error(decoder.decode(getUint8Slice(ptr, len)));
			},
			__wbg_getRandomValues_cc7f052a444bb2ce(ptr, len) {
				try {
					globalThis.crypto.getRandomValues(getUint8Slice(ptr, len));
				} catch (error) {
					storeException(error);
				}
			},
			__wbg_new_from_slice_ddf8b82c4d6af38e(ptr, len) {
				let output = frameOutputs.get(len);
				if (!output) {
					output = new Float32Array(len);
					frameOutputs.set(len, output);
				}
				output.set(getFloat32Slice(ptr, len));
				return output;
			},
			__wbindgen_init_externref_table() {
				const table = wasm.__wbindgen_externrefs;
				const offset = table.grow(4);
				table.set(0, undefined);
				table.set(offset + 0, undefined);
				table.set(offset + 1, null);
				table.set(offset + 2, true);
				table.set(offset + 3, false);
			},
		},
	};

	const instance = new WebAssembly.Instance(module, imports);
	wasm = instance.exports;
	wasm.__wbindgen_start();

	return {
		create(modelBytes, attenLimDb) {
			const ptr = passUint8Array(modelBytes);
			return wasm.df_create(ptr, vectorLength, attenLimDb) >>> 0;
		},
		frameLength(handle) {
			return wasm.df_get_frame_length(handle) >>> 0;
		},
		processFrame(handle, input) {
			const ptr = passFloat32Array(input);
			return wasm.df_process_frame(handle, ptr, vectorLength);
		},
		free(handle) {
			wasm.__wbg_dfstate_free(handle, 1);
		},
	};
}

function createHighPass(cutoffHz, rate) {
	if (!(cutoffHz > 0)) return null;
	const omega = (2 * Math.PI * cutoffHz) / rate;
	const alpha = Math.sin(omega) / (2 * Math.SQRT1_2);
	const cos = Math.cos(omega);
	const a0 = 1 + alpha;
	return {
		b0: (1 + cos) / 2 / a0,
		b1: -(1 + cos) / a0,
		b2: (1 + cos) / 2 / a0,
		a1: (-2 * cos) / a0,
		a2: (1 - alpha) / a0,
		x1: 0,
		x2: 0,
		y1: 0,
		y2: 0,
	};
}

class DeepFilterProcessor extends AudioWorkletProcessor {
	constructor(options) {
		super();
		const config = options.processorOptions;
		this.inputGain = config.inputGain;
		this.outputGain = config.outputGain;
		this.highPassHz = config.highPassHz;
		this.highPass = createHighPass(this.highPassHz, sampleRate);
		this.bindings = null;
		this.handle = 0;
		this.frameLength = 0;
		this.running = false;
		this.disposed = false;
		this.primed = false;
		this.nonFiniteFrames = 0;
		this.healthFrames = 0;
		this.healthInputEnergy = 0;
		this.healthOutputEnergy = 0;
		this.healthSamples = 0;
		this.healthNonFinite = false;
		this.processedFrames = 0;
		this.silence = new Float32Array(RENDER_QUANTUM);
		this.port.onmessage = (event) => this.handleMessage(event.data);
		try {
			this.bindings = createDeepFilterBindings(config.wasmModule);
			this.handle = this.bindings.create(new Uint8Array(config.modelBytes), config.attenLimDb);
			this.frameLength = this.bindings.frameLength(this.handle);
			this.ringSize = this.frameLength * 4;
			this.inputRing = new Float32Array(this.ringSize);
			this.outputRing = new Float32Array(this.ringSize);
			this.frame = new Float32Array(this.frameLength);
			for (let i = 0; i < WARM_UP_FRAMES; i++) this.bindings.processFrame(this.handle, this.frame);
			this.resetRings();
			this.running = true;
			this.port.postMessage({type: 'ready', frameLength: this.frameLength});
		} catch (error) {
			this.port.postMessage({type: 'error', message: String(error?.message ?? error)});
		}
	}

	resetRings() {
		this.inputRing.fill(0);
		this.outputRing.fill(0);
		this.inputWrite = 0;
		this.inputRead = 0;
		this.outputWrite = 0;
		this.outputRead = 0;
		this.primed = false;
		if (this.highPass) {
			this.highPass.x1 = 0;
			this.highPass.x2 = 0;
			this.highPass.y1 = 0;
			this.highPass.y2 = 0;
		}
	}

	handleMessage(message) {
		if (!message || typeof message !== 'object') return;
		if (message.type === 'flush' && this.running) {
			this.resetRings();
			return;
		}
		if (message.type === 'dispose') this.dispose();
	}

	dispose() {
		if (this.disposed) return;
		this.disposed = true;
		this.running = false;
		if (this.bindings && this.handle) {
			try {
				this.bindings.free(this.handle);
			} catch {}
		}
		this.bindings = null;
		this.handle = 0;
	}

	fail(message) {
		this.running = false;
		this.port.postMessage({type: 'error', message});
	}

	available(write, read) {
		return (write - read + this.ringSize) % this.ringSize;
	}

	writeInput(input) {
		const highPass = this.highPass;
		const gain = this.inputGain;
		for (let i = 0; i < input.length; i++) {
			let sample = input[i];
			if (!Number.isFinite(sample)) {
				sample = 0;
				this.healthNonFinite = true;
			}
			this.healthInputEnergy += sample * sample;
			if (highPass) {
				const filtered =
					highPass.b0 * sample +
					highPass.b1 * highPass.x1 +
					highPass.b2 * highPass.x2 -
					highPass.a1 * highPass.y1 -
					highPass.a2 * highPass.y2;
				highPass.x2 = highPass.x1;
				highPass.x1 = sample;
				highPass.y2 = highPass.y1;
				highPass.y1 = filtered;
				sample = filtered;
			}
			this.inputRing[this.inputWrite] = sample * gain;
			this.inputWrite = (this.inputWrite + 1) % this.ringSize;
		}
		this.healthSamples += input.length;
	}

	processFrames() {
		const frameLength = this.frameLength;
		while (this.available(this.inputWrite, this.inputRead) >= frameLength) {
			for (let i = 0; i < frameLength; i++) {
				this.frame[i] = this.inputRing[this.inputRead];
				this.inputRead = (this.inputRead + 1) % this.ringSize;
			}
			const processed = this.bindings.processFrame(this.handle, this.frame);
			let finite = true;
			for (let i = 0; i < processed.length; i++) {
				if (!Number.isFinite(processed[i])) {
					finite = false;
					break;
				}
			}
			if (finite) {
				this.nonFiniteFrames = 0;
			} else {
				this.nonFiniteFrames++;
				this.healthNonFinite = true;
				if (this.nonFiniteFrames >= MAX_NON_FINITE_FRAMES) {
					this.fail('DeepFilterNet produced non-finite output');
					return;
				}
			}
			for (let i = 0; i < processed.length; i++) {
				const sample = finite ? processed[i] * this.outputGain : 0;
				this.healthOutputEnergy += sample * sample;
				this.outputRing[this.outputWrite] = sample;
				this.outputWrite = (this.outputWrite + 1) % this.ringSize;
			}
			this.processedFrames++;
			this.healthFrames++;
			if (this.healthFrames >= HEALTH_REPORT_FRAMES) this.reportHealth();
		}
	}

	reportHealth() {
		const samples = Math.max(1, this.healthSamples);
		this.port.postMessage({
			type: 'health',
			inputRms: Math.sqrt(this.healthInputEnergy / samples),
			outputRms: Math.sqrt(this.healthOutputEnergy / samples),
			nonFinite: this.healthNonFinite,
			frame: this.processedFrames,
			contextTime: currentTime,
		});
		this.healthFrames = 0;
		this.healthInputEnergy = 0;
		this.healthOutputEnergy = 0;
		this.healthSamples = 0;
		this.healthNonFinite = false;
	}

	process(inputs, outputs) {
		if (this.disposed) return false;
		const input = inputs[0]?.[0] ?? this.silence;
		const output = outputs[0]?.[0];
		if (!output) return true;
		if (!this.running) {
			output.set(input);
			return true;
		}
		try {
			this.writeInput(input);
			this.processFrames();
		} catch (error) {
			this.fail(String(error?.message ?? error));
			output.set(input);
			return true;
		}
		if (!this.running) {
			output.set(input);
			return true;
		}
		if (this.available(this.outputWrite, this.outputRead) < RENDER_QUANTUM) return true;
		for (let i = 0; i < RENDER_QUANTUM; i++) {
			output[i] = this.outputRing[this.outputRead];
			this.outputRead = (this.outputRead + 1) % this.ringSize;
		}
		if (!this.primed) {
			this.primed = true;
			this.port.postMessage({type: 'primed'});
		}
		return true;
	}
}

registerProcessor(PROCESSOR_NAME, DeepFilterProcessor);
