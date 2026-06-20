// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';

function encodeIpcMessage(type, data) {
	const dataStr = JSON.stringify(data);
	const dataSize = Buffer.byteLength(dataStr);
	const buf = Buffer.allocUnsafe(dataSize + 8);
	buf.writeInt32LE(type, 0);
	buf.writeInt32LE(dataSize, 4);
	buf.write(dataStr, 8, dataSize);
	return buf;
}

const payload = {cmd: 'SET_ACTIVITY', args: {activity: {details: 'test'}}};
const frame = encodeIpcMessage(1, payload);
assert.equal(frame.readInt32LE(0), 1);
assert.equal(frame.readInt32LE(4), Buffer.byteLength(JSON.stringify(payload)));
assert.deepEqual(JSON.parse(frame.subarray(8).toString()), payload);
console.log('ArRpcServer framing test passed');
