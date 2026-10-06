// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {test} from 'vitest';
import {getEncryptedFrameLayout, isFrameServerInjected} from './FrameCryptor.ts';

const sifTrailer = Uint8Array.from([0xde, 0xad, 0xbe, 0xef]);
const shortFrame = new Uint8Array(3);
const encryptedFrame = makeEncryptedFrame();

function makeEncryptedFrame(): Uint8Array {
	const frame = new Uint8Array(1500);
	frame[frame.length - 2] = 12;
	frame[frame.length - 1] = 0;
	return frame;
}

test('FrameCryptor frame guards', async ({bench}) => {
	await bench('valid encrypted frame layout', () => {
		getEncryptedFrameLayout(encryptedFrame.buffer, 10);
	}).run();
	await bench('short encrypted frame layout rejection', () => {
		getEncryptedFrameLayout(shortFrame.buffer, 10);
	}).run();
	await bench('short SIF trailer rejection', () => {
		isFrameServerInjected(shortFrame.buffer, sifTrailer);
	}).run();
});
