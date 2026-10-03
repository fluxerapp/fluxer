// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const keys = loadTsModule('@electron/main/GlobalShortcutKeys');

function combo(overrides) {
	return {key: '', ctrl: false, alt: false, shift: false, meta: false, ...overrides};
}

describe('GlobalShortcutKeys physical tables', () => {
	test('evdev KEY codes map to DOM codes', () => {
		assert.equal(keys.evdevKeyToDomCode(30), 'KeyA');
		assert.equal(keys.evdevKeyToDomCode(50), 'KeyM');
		assert.equal(keys.evdevKeyToDomCode(3), 'Digit2');
		assert.equal(keys.evdevKeyToDomCode(57), 'Space');
		assert.equal(keys.evdevKeyToDomCode(97), 'ControlRight');
		assert.equal(keys.evdevKeyToDomCode(183), 'F13');
		assert.equal(keys.evdevKeyToDomCode(0x7fff), null);
	});

	test('every key the native evdev keymap names resolves to the same DOM code in main', () => {
		const source = readFileSync(new URL('../../native/linux-evdev/src/keymap.rs', import.meta.url), 'utf8');
		const table = source.slice(source.indexOf('pub const KEY_MAP'), source.indexOf('];'));
		const entries = [...table.matchAll(/\((\d+), "([^"]+)"\)/g)].map((match) => [Number(match[1]), match[2]]);
		assert.ok(entries.length > 100);
		for (const [keycode, name] of entries) {
			const code = keys.evdevKeyToDomCode(keycode);
			assert.notEqual(code, null, `evdev ${keycode} ${name}`);
			assert.equal(code, keys.hookKeyNameToDomCode(name), `evdev ${keycode} ${name}`);
			assert.equal(keys.x11KeycodeToDomCode(keycode + 8), code, `x11 ${keycode + 8} ${name}`);
		}
		assert.equal(keys.evdevKeyToDomCode(148), 'LaunchApp1');
		assert.equal(keys.evdevKeyToDomCode(149), 'LaunchApp2');
		assert.equal(keys.evdevKeyToDomCode(226), 'MediaSelect');
		assert.equal(keys.evdevKeyToDomCode(364), 'BrowserFavorites');
	});

	test('X11 keycodes are evdev codes offset by eight', () => {
		assert.equal(keys.x11KeycodeToDomCode(38), 'KeyA');
		assert.equal(keys.x11KeycodeToDomCode(58), 'KeyM');
		assert.equal(keys.x11KeycodeToDomCode(11), 'Digit2');
		assert.equal(keys.x11KeycodeToDomCode(65), 'Space');
		assert.equal(keys.x11KeycodeToDomCode(4), null);
	});

	test('Windows keys use the scan code with the extended flag', () => {
		assert.equal(keys.windowsKeyToDomCode(0x1e, false, 0x41), 'KeyA');
		assert.equal(keys.windowsKeyToDomCode(0x32, false, 0x4d), 'KeyM');
		assert.equal(keys.windowsKeyToDomCode(0x1d, false, 0xa2), 'ControlLeft');
		assert.equal(keys.windowsKeyToDomCode(0x1d, true, 0xa3), 'ControlRight');
		assert.equal(keys.windowsKeyToDomCode(0x1c, true, 0x0d), 'NumpadEnter');
		assert.equal(keys.windowsKeyToDomCode(0x45, false, 0x13), 'Pause');
		assert.equal(keys.windowsKeyToDomCode(0x45, true, 0x90), 'NumLock');
		assert.equal(keys.windowsKeyToDomCode(0x54, false, 0x2c), 'PrintScreen');
		assert.equal(keys.windowsKeyToDomCode(0x46, true, 0x13), 'Pause');
	});

	test('Windows keys fall back to the virtual key when the scan code is missing or unknown', () => {
		assert.equal(keys.windowsKeyToDomCode(0, false, 0x7c), 'F13');
		assert.equal(keys.windowsKeyToDomCode(0, false, 0xa0), 'ShiftLeft');
		assert.equal(keys.windowsKeyToDomCode(0x7f, false, 0x26), 'ArrowUp');
		assert.equal(keys.windowsKeyToDomCode(0, false, 0x5a), 'KeyZ');
		assert.equal(keys.windowsKeyToDomCode(0, false, 0x39), 'Digit9');
		assert.equal(keys.windowsKeyToDomCode(0, false, 0x87), 'F24');
		assert.equal(keys.windowsKeyToDomCode(0, false, 0xff), null);
	});

	test('macOS virtual keycodes map to DOM codes', () => {
		assert.equal(keys.macosKeycodeToDomCode(0x00), 'KeyA');
		assert.equal(keys.macosKeycodeToDomCode(0x2e), 'KeyM');
		assert.equal(keys.macosKeycodeToDomCode(0x31), 'Space');
		assert.equal(keys.macosKeycodeToDomCode(0x37), 'MetaLeft');
		assert.equal(keys.macosKeycodeToDomCode(0x69), 'F13');
	});
});

describe('GlobalShortcutKeys layout names', () => {
	test('layout keys normalise to hook names', () => {
		assert.equal(keys.hookKeyNameForLayoutKey('m'), 'M');
		assert.equal(keys.hookKeyNameForLayoutKey('2'), '2');
		assert.equal(keys.hookKeyNameForLayoutKey('-'), 'Minus');
		assert.equal(keys.hookKeyNameForLayoutKey(' '), 'Space');
		assert.equal(keys.hookKeyNameForLayoutKey('é'), null);
	});

	test('legacy hook names convert to DOM codes', () => {
		assert.equal(keys.hookKeyNameToDomCode('M'), 'KeyM');
		assert.equal(keys.hookKeyNameToDomCode('1'), 'Digit1');
		assert.equal(keys.hookKeyNameToDomCode('Esc'), 'Escape');
		assert.equal(keys.hookKeyNameToDomCode('NumpadEnter'), 'NumpadEnter');
		assert.equal(keys.hookKeyNameToDomCode('Key291'), null);
		assert.equal(keys.hookKeyNameToDomCode('LaunchMediaPlayer'), 'MediaSelect');
	});

	test('US layout keys map to physical codes', () => {
		assert.equal(keys.usLayoutKeyToDomCode('m'), 'KeyM');
		assert.equal(keys.usLayoutKeyToDomCode('M'), 'KeyM');
		assert.equal(keys.usLayoutKeyToDomCode('/'), 'Slash');
		assert.equal(keys.usLayoutKeyToDomCode('F5'), 'F5');
		assert.equal(keys.usLayoutKeyToDomCode('ArrowUp'), 'ArrowUp');
	});

	test('printable non-numpad keys prefer the layout key', () => {
		assert.equal(keys.shouldPreferLayoutKey({key: 'm', code: 'KeyM'}), true);
		assert.equal(keys.shouldPreferLayoutKey({key: '1', code: 'Numpad1'}), false);
		assert.equal(keys.shouldPreferLayoutKey({key: 'F5', code: 'F5'}), false);
		assert.equal(keys.shouldPreferLayoutKey({key: 'Dead', code: 'BracketLeft'}), false);
	});
});

describe('comboToXdgTrigger', () => {
	test('uses uppercase modifiers and keysym names', () => {
		assert.equal(keys.comboToXdgTrigger(combo({key: 'm', code: 'KeyM', ctrl: true, shift: true})), 'CTRL+SHIFT+m');
		assert.equal(
			keys.comboToXdgTrigger(combo({key: 'a', ctrl: true, alt: true, shift: true, meta: true})),
			'CTRL+ALT+SHIFT+LOGO+a',
		);
		assert.equal(keys.comboToXdgTrigger(combo({key: 'F13', code: 'F13'})), 'F13');
		assert.equal(keys.comboToXdgTrigger(combo({key: ' ', code: 'Space'})), 'space');
		assert.equal(keys.comboToXdgTrigger(combo({key: 'PageDown', code: 'PageDown', alt: true})), 'ALT+Next');
		assert.equal(keys.comboToXdgTrigger(combo({key: '-', code: 'Minus', ctrl: true})), 'CTRL+minus');
		assert.equal(keys.comboToXdgTrigger(combo({key: 'AudioVolumeMute', code: 'AudioVolumeMute'})), 'XF86AudioMute');
		assert.equal(keys.comboToXdgTrigger(combo({key: 'MediaPlayPause', code: 'MediaPlayPause'})), 'XF86AudioPlay');
	});

	test('letters follow the layout and everything else follows the physical key', () => {
		assert.equal(keys.comboToXdgTrigger(combo({key: 'z', code: 'KeyY', ctrl: true})), 'CTRL+z');
		assert.equal(keys.comboToXdgTrigger(combo({key: '!', code: 'Digit1', shift: true})), 'SHIFT+1');
		assert.equal(keys.comboToXdgTrigger(combo({key: ';', code: 'Comma', shift: true})), 'SHIFT+comma');
		assert.equal(keys.comboToXdgTrigger(combo({key: 'ä', code: 'Quote'})), 'apostrophe');
		assert.equal(keys.comboToXdgTrigger(combo({key: ',', code: 'Comma'})), 'comma');
	});

	test('returns null for combos a portal cannot express', () => {
		assert.equal(keys.comboToXdgTrigger(combo({key: '', mouseButton: 3})), null);
		assert.equal(
			keys.comboToXdgTrigger(combo({key: 'Shift', code: 'ShiftLeft', shift: true, modifierOnly: true})),
			null,
		);
		assert.equal(
			keys.comboToXdgTrigger(
				combo({key: 'Control', code: 'ControlLeft', ctrl: true, modifierOnly: true, bothSides: true}),
			),
			null,
		);
		assert.equal(keys.comboToXdgTrigger(combo({key: ''})), null);
		assert.equal(keys.comboToXdgTrigger(combo({key: 'Control', code: 'ControlLeft'})), null);
		assert.equal(keys.comboToXdgTrigger(combo({key: 'NumLock', code: 'NumLock'})), null);
		assert.equal(keys.comboToXdgTrigger(combo({key: 'CapsLock', code: 'CapsLock'})), null);
		assert.equal(keys.comboToXdgTrigger(combo({key: '1', code: 'Numpad1'})), null);
		assert.equal(keys.comboToXdgTrigger(combo({key: 'a', code: 'NumpadAdd'})), null);
		assert.equal(keys.comboToXdgTrigger(combo({key: '<', code: 'IntlBackslash'})), null);
		assert.equal(keys.comboToXdgTrigger(combo({key: 'é', code: 'Unknown'})), null);
		assert.equal(keys.comboToXdgTrigger(combo({key: 'AudioVolumeUp', code: 'AudioVolumeUp'})), null);
	});

	test('every emitted key name is a plain keysym identifier', () => {
		const codes = [
			...Array.from({length: 0x250}, (_, index) => keys.evdevKeyToDomCode(index)),
			...Array.from({length: 0x100}, (_, index) => keys.windowsKeyToDomCode(0, false, index)),
		].filter((code) => code !== null);
		const pattern = /^(?:(?:CTRL|ALT|SHIFT|LOGO)\+)*[A-Za-z0-9_]+$/;
		let emitted = 0;
		for (const code of codes) {
			for (const key of [code, 'x', '?', ' ']) {
				const trigger = keys.comboToXdgTrigger(combo({key, code, ctrl: true}));
				if (trigger === null) continue;
				emitted += 1;
				assert.match(trigger, pattern, `${key}/${code}`);
				assert.ok(!/NUM|CAPS|KP_/i.test(trigger.split('+').at(-1)), trigger);
			}
		}
		assert.ok(emitted > 100);
	});
});
