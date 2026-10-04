// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {GuildEventImageReader} from '@app/features/guild/utils/GuildEventImageReader';
import {it} from 'vitest';

class ControlledReader {
	static readonly LOADING = 1;
	static instances: Array<ControlledReader> = [];
	readyState = 0;
	result: string | ArrayBuffer | null = null;
	error: Error | null = null;
	onload: (() => void) | null = null;
	onerror: (() => void) | null = null;
	onabort: (() => void) | null = null;
	aborts = 0;

	constructor() {
		ControlledReader.instances.push(this);
	}
	readAsDataURL(file: File): void {
		if (file.name === 'throw') throw new Error('read start failed');
		this.readyState = 1;
	}
	abort(): void {
		this.aborts++;
		this.readyState = 2;
		this.onabort?.();
	}
	load(value: string | null): void {
		this.result = value;
		this.readyState = 2;
		this.onload?.();
	}
}

function fixture(
	run: (
		reader: GuildEventImageReader,
		loads: Array<string>,
		errors: Array<string>,
		pending: Array<boolean>,
	) => void,
): void {
	const previous = globalThis.FileReader;
	globalThis.FileReader = ControlledReader as unknown as typeof FileReader;
	ControlledReader.instances = [];
	const loads: Array<string> = [];
	const errors: Array<string> = [];
	const pending: Array<boolean> = [];
	const reader = new GuildEventImageReader({
		onLoad: (image) => loads.push(image),
		onError: (error) => errors.push(error.message),
		onPending: (value) => pending.push(value),
	});
	try {
		run(reader, loads, errors, pending);
	} finally {
		reader.cancel(false);
		if (previous === undefined) Reflect.deleteProperty(globalThis, 'FileReader');
		else globalThis.FileReader = previous;
	}
}
const file = {name: 'event.png'} as File;

it('marks the selected image pending until its bytes arrive', () => {
	fixture((reader, loads, errors, pending) => {
		reader.read(file);
		assert.equal(pending.at(-1), true);
		ControlledReader.instances[0].load('data:image/png;base64,new');
		assert.deepEqual(loads, ['data:image/png;base64,new']);
		assert.equal(pending.at(-1), false);
		assert.deepEqual(errors, []);
		assert.equal(ControlledReader.instances[0].onload, null);
	});
});

it('aborts older selections and ignores already-queued older callbacks', () => {
	fixture((reader, loads) => {
		reader.read(file);
		const first = ControlledReader.instances[0];
		const queued = first.onload!;
		reader.read(file);
		assert.equal(first.aborts, 1);
		ControlledReader.instances[1].load('newest');
		first.result = 'obsolete';
		queued();
		assert.deepEqual(loads, ['newest']);
	});
});

it('reset, edit-switch and image-removal cancellation block stale draft writes', () => {
	fixture((reader, loads, _errors, pending) => {
		reader.read(file);
		const native = ControlledReader.instances[0];
		const queued = native.onload!;
		reader.cancel();
		native.result = 'obsolete';
		queued();
		assert.equal(native.aborts, 1);
		assert.equal(pending.at(-1), false);
		assert.deepEqual(loads, []);
	});
});

it('surfaces read failures and permits a new selection afterward', () => {
	fixture((reader, loads, errors, pending) => {
		reader.read(file);
		const native = ControlledReader.instances[0];
		native.error = new Error('image unavailable');
		native.onerror!();
		assert.deepEqual(errors, ['image unavailable']);
		assert.equal(pending.at(-1), false);
		reader.read(file);
		ControlledReader.instances[1].load('replacement');
		assert.deepEqual(loads, ['replacement']);
	});
});

it('handles synchronous read-start errors without leaving Save pending', () => {
	fixture((reader, _loads, errors, pending) => {
		assert.doesNotThrow(() => reader.read({name: 'throw'} as File));
		assert.deepEqual(errors, ['read start failed']);
		assert.equal(pending.at(-1), false);
	});
});

it('unmount cancellation aborts without notifying component state', () => {
	fixture((reader, loads, errors, pending) => {
		reader.read(file);
		const before = pending.length;
		reader.cancel(false);
		assert.equal(ControlledReader.instances[0].aborts, 1);
		assert.equal(pending.length, before);
		assert.deepEqual(loads, []);
		assert.deepEqual(errors, []);
	});
});

it('never treats a non-text result as selected image data', () => {
	fixture((reader, loads, errors, pending) => {
		reader.read(file);
		ControlledReader.instances[0].load(null);
		assert.deepEqual(loads, []);
		assert.deepEqual(errors, ['Unable to read image']);
		assert.equal(pending.at(-1), false);
	});
});

it('keeps only one read outstanding across repeated selections', () => {
	fixture((reader) => {
		for (let index = 0; index < 20; index++) {
			reader.read(file);
			assert.equal(ControlledReader.instances.filter((native) => native.readyState === 1).length, 1);
		}
		assert.equal(ControlledReader.instances.reduce((total, native) => total + native.aborts, 0), 19);
	});
});
