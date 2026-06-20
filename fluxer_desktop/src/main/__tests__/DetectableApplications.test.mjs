// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import detectables from '../../../assets/rpc/detectables.json' with {type: 'json'};

assert.ok(Array.isArray(detectables));
assert.ok(detectables.length >= 1);
const minecraft = detectables.find((entry) => entry.name === 'Minecraft');
assert.ok(minecraft?.url?.startsWith('https://'));
assert.ok(Array.isArray(minecraft?.executables));
assert.ok(minecraft?.executables?.some((exe) => exe.os === 'linux' && exe.name === '>java'));
console.log('DetectableApplications data test passed');
