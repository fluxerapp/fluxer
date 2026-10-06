// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const {GlobalShortcutMatcher, bindingFromCombo} = loadTsModule('@electron/main/GlobalShortcutMatcher');

function combo(overrides) {
	return {key: '', ctrl: false, alt: false, shift: false, meta: false, ...overrides};
}

function key(type, code, {key = null, raw, ctrl = false, alt = false, shift = false, meta = false} = {}) {
	return {type, code, key, rawKeycode: raw, ctrlKey: ctrl, altKey: alt, shiftKey: shift, metaKey: meta};
}

function mouse(type, button, mods = {}) {
	return {
		type,
		button,
		ctrlKey: mods.ctrl === true,
		altKey: mods.alt === true,
		shiftKey: mods.shift === true,
		metaKey: mods.meta === true,
	};
}

function matcherWith(...entries) {
	const matcher = new GlobalShortcutMatcher();
	matcher.setBindings(entries.map(([sourceId, action, value]) => bindingFromCombo(sourceId, action, combo(value))));
	return matcher;
}

function phases(transitions) {
	return transitions.map((transition) => `${transition.phase}:${transition.sourceId}`);
}

describe('GlobalShortcutMatcher key identity', () => {
	test('a physical backend never confuses a keysym with a binding code', () => {
		const matcher = matcherWith([
			'default:voice_toggle_mute',
			'voice_toggle_mute',
			{key: 'm', code: 'KeyM', ctrl: true, shift: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'Digit2', {raw: 3, ctrl: true, shift: true}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyM', {raw: 50, ctrl: true, shift: true}), true)), [
			'press:default:voice_toggle_mute',
		]);
	});

	test('a layout-aware backend matches printable keys by layout name', () => {
		const matcher = matcherWith([
			'default:voice_toggle_mute',
			'voice_toggle_mute',
			{key: 'm', code: 'Semicolon', ctrl: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyM', {key: 'Comma', raw: 58, ctrl: true}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyL', {key: 'L', raw: 59, ctrl: true}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'Semicolon', {key: 'M', raw: 47, ctrl: true}), true)), [
			'press:default:voice_toggle_mute',
		]);
	});

	test('a letter from another layout group falls back to the physical code', () => {
		const matcher = matcherWith(['custom:a', 'voice_toggle_mute', {key: 'q', code: 'KeyQ', ctrl: true}]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyQ', {key: null, raw: 24, ctrl: true}), true)), [
			'press:custom:a',
		]);
	});

	test('a letter whose hook name is not a single character falls back to the physical code', () => {
		const matcher = matcherWith(['custom:a', 'voice_toggle_mute', {key: 'q', code: 'KeyQ', ctrl: true}]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyQ', {key: 'Oem7', raw: 24, ctrl: true}), true)), [
			'press:custom:a',
		]);
	});

	test('punctuation matches by physical code whatever the hook calls the key', () => {
		const matcher = matcherWith(
			['custom:less', 'voice_toggle_mute', {key: '<', code: 'IntlBackslash'}],
			['custom:semicolon', 'voice_toggle_deafen', {key: ';', code: 'Comma', shift: true}],
		);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'Comma', {key: 'Comma', raw: 0x33}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'IntlBackslash', {key: 'Comma', raw: 0x56}), true)), [
			'press:custom:less',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'Comma', {key: 'Comma', raw: 0x33, shift: true}), true)), [
			'press:custom:semicolon',
		]);
	});

	test('a letter binding without a code ignores the physical key that types punctuation', () => {
		const matcher = matcherWith([
			'default:voice_toggle_mute',
			'voice_toggle_mute',
			{key: 'm', ctrl: true, shift: true},
		]);
		assert.deepEqual(
			phases(matcher.handleKey(key('keydown', 'KeyM', {key: 'Comma', raw: 50, ctrl: true, shift: true}), true)),
			[],
		);
		assert.deepEqual(
			phases(matcher.handleKey(key('keydown', 'Semicolon', {key: 'M', raw: 39, ctrl: true, shift: true}), true)),
			['press:default:voice_toggle_mute'],
		);
	});

	test('a key-only binding falls back to the US position on physical backends', () => {
		const matcher = matcherWith([
			'default:voice_toggle_deafen',
			'voice_toggle_deafen',
			{key: 'd', ctrl: true, shift: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyD', {raw: 32, ctrl: true, shift: true}), true)), [
			'press:default:voice_toggle_deafen',
		]);
	});

	test('a named key binding that uses its hook name as the layout key still matches', () => {
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([
			{
				sourceId: 'key:voice_push_to_talk::F8:F8',
				action: 'key:voice_push_to_talk::F8:F8',
				trigger: {kind: 'key', layoutKey: 'F8', code: 'F8'},
				modifiers: {ctrl: false, alt: false, shift: false, meta: false},
			},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'F8', {key: 'F8', raw: 74}), true)), [
			'press:key:voice_push_to_talk::F8:F8',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'F8', {key: 'F8', raw: 74}), true)), [
			'release:key:voice_push_to_talk::F8:F8',
		]);
	});

	test('non-printable and numpad keys match by code even on layout-aware backends', () => {
		const matcher = matcherWith(
			['custom:a', 'voice_push_to_talk', {key: 'F13', code: 'F13'}],
			['custom:b', 'voice_disconnect', {key: '1', code: 'Numpad1'}],
		);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'F13', {key: 'F13', raw: 191}), true)), [
			'press:custom:a',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'Digit1', {key: '1', raw: 10}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'Numpad1', {key: 'Numpad1', raw: 87}), true)), [
			'press:custom:b',
		]);
	});
});

describe('GlobalShortcutMatcher modifiers', () => {
	test('bindings with modifiers need an exact modifier match', () => {
		const matcher = matcherWith(['custom:a', 'voice_toggle_mute', {key: 'F5', code: 'F5', ctrl: true}]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'F5', {raw: 63, ctrl: true, shift: true}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'F5', {raw: 63}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'F5', {raw: 63, ctrl: true}), true)), ['press:custom:a']);
	});

	test('bindings without modifiers fire with extra modifiers held, for toggles and holds alike', () => {
		const matcher = matcherWith(
			['custom:toggle', 'voice_toggle_mute', {key: 'F6', code: 'F6'}],
			['custom:hold', 'voice_push_to_talk', {key: 'F7', code: 'F7'}],
		);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'F6', {raw: 64, shift: true}), true)), [
			'press:custom:toggle',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'F7', {raw: 65, ctrl: true, alt: true}), true)), [
			'press:custom:hold',
		]);
	});

	test('release follows the main key regardless of modifier order', () => {
		const matcher = matcherWith(['custom:a', 'voice_push_to_talk', {key: ' ', code: 'Space', ctrl: true}]);
		matcher.handleKey(key('keydown', 'ControlLeft', {raw: 29}), true);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'Space', {raw: 57, ctrl: true}), true)), [
			'press:custom:a',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'ControlLeft', {raw: 29}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'Space', {raw: 57}), true)), ['release:custom:a']);
	});

	test('autorepeat never emits a second press', () => {
		const matcher = matcherWith(['custom:a', 'voice_push_to_talk', {key: 'F8', code: 'F8'}]);
		assert.equal(matcher.handleKey(key('keydown', 'F8', {raw: 66}), true).length, 1);
		assert.equal(matcher.handleKey(key('keydown', 'F8', {raw: 66}), true).length, 0);
		assert.equal(matcher.handleKey(key('keyup', 'F8', {raw: 66}), true).length, 1);
	});
});

describe('GlobalShortcutMatcher modifier-only combos', () => {
	test('presses when every required modifier is down and releases when one goes up', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'Control', code: 'ControlLeft', ctrl: true, alt: true, modifierOnly: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ControlLeft', {raw: 29}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'AltRight', {raw: 100, ctrl: true}), true)), [
			'press:custom:a',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'AltRight', {raw: 100}), true)), ['release:custom:a']);
	});

	test('bothSides requires the left and right key of the modifier', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'Shift', code: 'ShiftLeft', shift: true, modifierOnly: true, bothSides: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ShiftLeft', {raw: 42}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ShiftRight', {raw: 54, shift: true}), true)), [
			'press:custom:a',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'ShiftLeft', {raw: 42, shift: true}), true)), [
			'release:custom:a',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'ShiftRight', {raw: 54}), true)), []);
	});

	test('a modifier release reported as a keydown still releases once the flags clear', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'Shift', code: 'ShiftLeft', shift: true, modifierOnly: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ShiftLeft', {raw: 0x38, shift: true}), true)), [
			'press:custom:a',
		]);
		matcher.handleKey(key('keydown', 'ShiftRight', {raw: 0x3c, shift: true}), true);
		matcher.handleKey(key('keydown', 'ShiftRight', {raw: 0x3c, shift: true}), true);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'ShiftLeft', {raw: 0x38}), true)), ['release:custom:a']);
	});

	test('a lost modifier keyup is recovered from the flags of the next event', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'Control', code: 'ControlLeft', ctrl: true, modifierOnly: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ControlLeft', {raw: 29}), true)), ['press:custom:a']);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyA', {raw: 30}), true)), ['release:custom:a']);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ControlLeft', {raw: 29}), true)), ['press:custom:a']);
		assert.deepEqual(phases(matcher.handleMouse(mouse('mousedown', 1), true)), ['release:custom:a']);
	});

	test('a modifier code the backend cannot flag is kept until its own key-up', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'AltGraph', code: 'AltRight', modifierOnly: true},
		]);
		const unflaggedModifierCodes = ['AltRight'];
		assert.deepEqual(
			phases(matcher.handleKey({...key('keydown', 'AltRight', {raw: 108}), unflaggedModifierCodes}, true)),
			['press:custom:a'],
		);
		assert.deepEqual(phases(matcher.handleMouse({...mouse('mousedown', 0), unflaggedModifierCodes}, true)), []);
		assert.deepEqual(
			phases(matcher.handleKey({...key('keydown', 'KeyQ', {raw: 24}), unflaggedModifierCodes}, true)),
			[],
		);
		assert.deepEqual(phases(matcher.handleKey({...key('keyup', 'KeyQ', {raw: 24}), unflaggedModifierCodes}, true)), []);
		assert.deepEqual(
			phases(matcher.handleKey({...key('keyup', 'AltRight', {raw: 108}), unflaggedModifierCodes}, true)),
			['release:custom:a'],
		);
	});

	test('AltGr reported with Ctrl and Alt flags keeps a Right Alt hold', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'AltGraph', code: 'AltRight', modifierOnly: true},
		]);
		matcher.handleKey(key('keydown', 'ControlLeft', {raw: 0x1d}), true);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'AltRight', {raw: 0xe038, ctrl: true}), true)), [
			'press:custom:a',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyQ', {raw: 0x10, ctrl: true, alt: true}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'AltRight', {raw: 0xe038, ctrl: true, alt: true}), true)), [
			'release:custom:a',
		]);
	});

	test('a stuck modifier no longer satisfies a combined modifier-only binding', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'Alt', code: 'AltLeft', ctrl: true, alt: true, modifierOnly: true},
		]);
		matcher.handleKey(key('keydown', 'ControlLeft', {raw: 29, ctrl: true}), true);
		matcher.handleKey(key('keydown', 'KeyL', {raw: 38, meta: true}), true);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'AltLeft', {raw: 56, alt: true}), true)), []);
	});
});

describe('GlobalShortcutMatcher layout modifiers', () => {
	test('an X11 key the layout turns into Ctrl holds a Ctrl binding', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'Control', code: 'ControlLeft', ctrl: true, modifierOnly: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'CapsLock', {key: 'ControlLeft', raw: 66}), true)), [
			'press:custom:a',
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'KeyC', {key: 'C', raw: 54, ctrl: true}), true)), []);
		assert.deepEqual(
			phases(matcher.handleKey(key('keyup', 'CapsLock', {key: 'ControlLeft', raw: 66, ctrl: true}), true)),
			['release:custom:a'],
		);
	});

	test('a physical Ctrl key the layout turns into Caps Lock does not hold a Ctrl binding', () => {
		const matcher = matcherWith([
			'custom:a',
			'voice_push_to_talk',
			{key: 'Control', code: 'ControlLeft', ctrl: true, modifierOnly: true},
		]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ControlLeft', {key: 'CapsLock', raw: 37}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'ControlLeft', {key: 'CapsLock', raw: 37}), true)), []);
	});
});

describe('GlobalShortcutMatcher modifier-only kinds', () => {
	test('the trigger key counts as a required modifier alongside the flags', () => {
		const binding = bindingFromCombo(
			'custom:a',
			'voice_push_to_talk',
			combo({key: 'Shift', code: 'ShiftLeft', ctrl: true, modifierOnly: true}),
		);
		assert.deepEqual(binding.trigger, {kind: 'modifier-only', modifiers: ['ctrl', 'shift'], bothSides: false});
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([binding]);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ControlLeft', {raw: 29, ctrl: true}), true)), []);
		assert.deepEqual(phases(matcher.handleKey(key('keydown', 'ShiftLeft', {raw: 42, ctrl: true, shift: true}), true)), [
			'press:custom:a',
		]);
	});
});

describe('GlobalShortcutMatcher mouse buttons', () => {
	test('match by button with the same modifier rule and release on mouseup', () => {
		const matcher = matcherWith(['custom:a', 'voice_push_to_talk', {key: '', mouseButton: 4}]);
		assert.deepEqual(phases(matcher.handleMouse(mouse('mousedown', 3), true)), []);
		assert.deepEqual(phases(matcher.handleMouse(mouse('mousedown', 4, {shift: true}), true)), ['press:custom:a']);
		assert.deepEqual(phases(matcher.handleMouse(mouse('mouseup', 4), true)), ['release:custom:a']);
	});
});

describe('GlobalShortcutMatcher lifecycle', () => {
	test('a paused press is never recorded, so no release follows', () => {
		const matcher = matcherWith(['custom:a', 'voice_push_to_talk', {key: 'F9', code: 'F9'}]);
		assert.deepEqual(matcher.handleKey(key('keydown', 'F9', {raw: 67}), false), []);
		assert.deepEqual(matcher.handleKey(key('keyup', 'F9', {raw: 67}), true), []);
	});

	test('releases still flow while paused', () => {
		const matcher = matcherWith(['custom:a', 'voice_push_to_talk', {key: 'F9', code: 'F9'}]);
		matcher.handleKey(key('keydown', 'F9', {raw: 67}), true);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'F9', {raw: 67}), false)), ['release:custom:a']);
	});

	test('sync keeps unchanged pressed sources and releases removed or changed ones', () => {
		const matcher = new GlobalShortcutMatcher();
		const kept = bindingFromCombo('custom:kept', 'voice_push_to_talk', combo({key: 'F1', code: 'F1'}));
		const changed = bindingFromCombo('custom:changed', 'voice_push_to_mute', combo({key: 'F2', code: 'F2'}));
		const removed = bindingFromCombo('custom:removed', 'voice_toggle_mute', combo({key: 'F3', code: 'F3'}));
		matcher.setBindings([kept, changed, removed]);
		matcher.handleKey(key('keydown', 'F1', {raw: 59}), true);
		matcher.handleKey(key('keydown', 'F2', {raw: 60}), true);
		matcher.handleKey(key('keydown', 'F3', {raw: 61}), true);
		const released = matcher.setBindings([
			bindingFromCombo('custom:kept', 'voice_push_to_talk', combo({key: 'F1', code: 'F1'})),
			bindingFromCombo('custom:changed', 'voice_push_to_mute', combo({key: 'F4', code: 'F4'})),
		]);
		assert.deepEqual(phases(released).sort(), ['release:custom:changed', 'release:custom:removed']);
		assert.deepEqual(phases(matcher.handleKey(key('keyup', 'F1', {raw: 59}), true)), ['release:custom:kept']);
	});

	test('releaseAll releases every pressed source', () => {
		const matcher = matcherWith(
			['custom:a', 'voice_push_to_talk', {key: 'F1', code: 'F1'}],
			['custom:b', 'voice_push_to_mute', {key: '', mouseButton: 3}],
		);
		matcher.handleKey(key('keydown', 'F1', {raw: 59}), true);
		matcher.handleMouse(mouse('mousedown', 3), true);
		assert.deepEqual(phases(matcher.releaseAll()).sort(), ['release:custom:a', 'release:custom:b']);
		assert.deepEqual(matcher.releaseAll(), []);
	});

	test('combos without any key identity are dropped', () => {
		assert.equal(bindingFromCombo('custom:a', 'voice_push_to_talk', combo({key: ''})), null);
		assert.equal(bindingFromCombo('custom:a', 'voice_push_to_talk', combo({key: '', modifierOnly: true})), null);
	});
});
