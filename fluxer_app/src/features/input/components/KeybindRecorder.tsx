// SPDX-License-Identifier: AGPL-3.0-or-later

import KeybindManager from '@app/features/app/keybindings/KeybindManager';
import styles from '@app/features/input/components/KeybindRecorder.module.css';
import {
	beginGlobalKeyCapture,
	globalKeyEventToCombo,
	isGlobalKeyEventModifierKey,
} from '@app/features/input/components/KeybindRecorderCapture';
import type {KeybindCommand, KeyCombo} from '@app/features/input/state/InputKeybind';
import {isGamepadButtonPressed} from '@app/features/input/utils/GamepadButtonUtils';
import {isKeybindModifierKey} from '@app/features/input/utils/KeybindComboUtils';
import {formatKeyCombo} from '@app/features/input/utils/KeybindUtils';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {GlobalKeyEvent} from '@app/features/platform/types/Electron';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowCounterClockwiseIcon, KeyboardIcon, PencilSimpleIcon, TrashIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const RECORD_SHORTCUT_DESCRIPTOR = msg({
	message: 'Record shortcut',
	comment: 'Short label in the keyboard shortcuts keybind recorder. Keep it concise.',
});
const EDIT_KEYBOARD_SHORTCUT_DESCRIPTOR = msg({
	message: 'Edit keyboard shortcut',
	comment: 'Button or menu action label in the keyboard shortcuts keybind recorder. Keep it concise.',
});
const NO_SHORTCUT_SET_DESCRIPTOR = msg({
	message: 'No shortcut set',
	comment: 'Empty-state text in the keyboard shortcuts keybind recorder.',
});

interface KeybindRecorderProps {
	action: KeybindCommand;
	label: React.ReactNode;
	labelPlacement?: 'stacked' | 'inline';
	value: KeyCombo;
	defaultValue?: KeyCombo | null;
	disabled?: boolean;
	onChange: (combo: KeyCombo) => void;
	onClear?: () => void;
	onReset?: () => void;
	className?: string;
	'data-flx'?: string;
}

const combosEqual = (a: KeyCombo | null | undefined, b: KeyCombo | null | undefined): boolean => {
	if (!a && !b) return true;
	if (!a || !b) return false;
	return (
		a.key === b.key &&
		a.code === b.code &&
		!!a.ctrlOrMeta === !!b.ctrlOrMeta &&
		!!a.ctrl === !!b.ctrl &&
		!!a.alt === !!b.alt &&
		!!a.shift === !!b.shift &&
		!!a.meta === !!b.meta &&
		!!a.modifierOnly === !!b.modifierOnly &&
		!!a.bothSides === !!b.bothSides &&
		(a.mouseButton ?? null) === (b.mouseButton ?? null) &&
		(a.gamepadButton ?? null) === (b.gamepadButton ?? null)
	);
};
const MODIFIER_BOTH_SIDES_PAIRS: ReadonlyArray<readonly [string, string]> = [
	['ShiftLeft', 'ShiftRight'],
	['ControlLeft', 'ControlRight'],
	['AltLeft', 'AltRight'],
	['MetaLeft', 'MetaRight'],
];
const RECORDABLE_MOUSE_BUTTONS = new Set([0, 1, 2, 3, 4]);
const normalizeKeyForCombo = (key: string): string => {
	if (key === 'Spacebar') return ' ';
	if (key === 'Break') return 'Pause';
	return key;
};
const isMacPlatform = (): boolean => /Mac|iPod|iPhone|iPad/.test(navigator.platform);
const modifierStateToCombo = (
	event: KeyboardEvent | MouseEvent,
): Pick<KeyCombo, 'ctrlOrMeta' | 'ctrl' | 'alt' | 'shift' | 'meta'> => {
	const mac = isMacPlatform();
	const primaryModifierDown = mac ? event.metaKey : event.ctrlKey;
	return {
		ctrlOrMeta: primaryModifierDown || undefined,
		ctrl: (mac ? event.ctrlKey : false) || undefined,
		alt: event.altKey || undefined,
		shift: event.shiftKey || undefined,
		meta: (mac ? false : event.metaKey) || undefined,
	};
};
const reconstructModifiersFromPhysicallyHeldKeys = (
	heldCodes: ReadonlySet<string>,
): Pick<KeyCombo, 'ctrlOrMeta' | 'ctrl' | 'alt' | 'shift' | 'meta'> => {
	const mac = isMacPlatform();
	const ctrl = heldCodes.has('ControlLeft') || heldCodes.has('ControlRight');
	const alt = heldCodes.has('AltLeft') || heldCodes.has('AltRight');
	const shift = heldCodes.has('ShiftLeft') || heldCodes.has('ShiftRight');
	const meta = heldCodes.has('MetaLeft') || heldCodes.has('MetaRight');
	const primaryModifierDown = mac ? meta : ctrl;
	return {
		ctrlOrMeta: primaryModifierDown || undefined,
		ctrl: (mac ? ctrl : false) || undefined,
		alt: alt || undefined,
		shift: shift || undefined,
		meta: (mac ? false : meta) || undefined,
	};
};
const mergeModifiers = (
	a: Pick<KeyCombo, 'ctrlOrMeta' | 'ctrl' | 'alt' | 'shift' | 'meta'>,
	b: Pick<KeyCombo, 'ctrlOrMeta' | 'ctrl' | 'alt' | 'shift' | 'meta'>,
): Pick<KeyCombo, 'ctrlOrMeta' | 'ctrl' | 'alt' | 'shift' | 'meta'> => ({
	ctrlOrMeta: a.ctrlOrMeta || b.ctrlOrMeta || undefined,
	ctrl: a.ctrl || b.ctrl || undefined,
	alt: a.alt || b.alt || undefined,
	shift: a.shift || b.shift || undefined,
	meta: a.meta || b.meta || undefined,
});
const keyboardEventToCombo = (event: KeyboardEvent): KeyCombo => ({
	key: normalizeKeyForCombo(event.key),
	code: event.code,
	...modifierStateToCombo(event),
});
const modifierOnlyCombo = (event: KeyboardEvent): KeyCombo => ({
	key: normalizeKeyForCombo(event.key),
	code: event.code,
	...modifierStateToCombo(event),
	modifierOnly: true,
});

interface KeybindEditorPopoutProps {
	value: KeyCombo;
	defaultValue: KeyCombo | null;
	onSave: (combo: KeyCombo) => void;
	onClear?: () => void;
	onReset?: () => void;
	onClose: () => void;
}

const KeybindEditorPopout: React.FC<KeybindEditorPopoutProps> = ({
	value,
	defaultValue,
	onSave,
	onClear,
	onReset,
	onClose,
}) => {
	const {i18n} = useLingui();
	const [recording, setRecording] = useState(true);
	const [previewCombo, setPreviewCombo] = useState<KeyCombo | null>(null);
	const currentCombo = previewCombo ?? value;
	const displayValue = formatKeyCombo(currentCombo);
	const defaultDisplayValue = defaultValue ? formatKeyCombo(defaultValue) : null;
	const currentHasValue = !!(
		currentCombo?.key ||
		currentCombo?.code ||
		currentCombo?.mouseButton !== undefined ||
		currentCombo?.gamepadButton !== undefined
	);
	const currentIsModified = defaultValue ? !combosEqual(currentCombo, defaultValue) : false;
	useEffect(() => {
		KeybindManager.suspend();
		return () => {
			KeybindManager.resume();
		};
	}, []);
	const cancelRecording = useCallback(() => {
		setRecording(false);
		setPreviewCombo(null);
	}, []);
	const finishRecording = useCallback((combo: KeyCombo) => {
		setRecording(false);
		setPreviewCombo(combo);
	}, []);
	const startRecording = useCallback(() => {
		setPreviewCombo(null);
		setRecording(true);
	}, []);
	useEffect(() => {
		if (!recording) return;
		let committed = false;
		let sawNonModifier = false;
		let lastModifierCombo: KeyCombo | null = null;
		const heldModifierCodes = new Set<string>();
		const seenModifierCodes = new Set<string>();
		const commit = (combo: KeyCombo) => {
			if (committed) return;
			committed = true;
			const savedCombo: KeyCombo = {
				...combo,
				global: value.global,
				enabled: true,
			};
			onSave(savedCombo);
			finishRecording(savedCombo);
		};
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				event.preventDefault();
				event.stopPropagation();
				cancelRecording();
				return;
			}
			event.preventDefault();
			event.stopPropagation();
			if (isKeybindModifierKey(event.key)) {
				heldModifierCodes.add(event.code);
				seenModifierCodes.add(event.code);
				const combo = keyboardEventToCombo(event);
				lastModifierCombo = {...combo, modifierOnly: true};
				setPreviewCombo(combo);
				return;
			}
			const baseCombo = keyboardEventToCombo(event);
			const combo: KeyCombo = {
				...baseCombo,
				...mergeModifiers(baseCombo, reconstructModifiersFromPhysicallyHeldKeys(heldModifierCodes)),
			};
			setPreviewCombo(combo);
			if (!combo.key && !combo.code) return;
			sawNonModifier = true;
			commit(combo);
		};
		const handleKeyUp = (event: KeyboardEvent) => {
			if (!isKeybindModifierKey(event.key)) return;
			if (sawNonModifier) return;
			heldModifierCodes.delete(event.code);
			if (heldModifierCodes.size > 0) return;
			const combo = lastModifierCombo ?? modifierOnlyCombo(event);
			if (!combo.key && !combo.code) return;
			const modifierFlagCount =
				(combo.shift ? 1 : 0) +
				(combo.ctrl ? 1 : 0) +
				(combo.alt ? 1 : 0) +
				(combo.meta ? 1 : 0) +
				(combo.ctrlOrMeta ? 1 : 0);
			if (modifierFlagCount === 1) {
				for (const [leftCode, rightCode] of MODIFIER_BOTH_SIDES_PAIRS) {
					if (seenModifierCodes.has(leftCode) && seenModifierCodes.has(rightCode)) {
						commit({...combo, code: leftCode, bothSides: true});
						return;
					}
				}
			}
			commit(combo);
		};
		const handleGlobalKeyEvent = (event: GlobalKeyEvent) => {
			if (committed) return;
			const baseCombo = globalKeyEventToCombo(event);
			if (!baseCombo) return;
			const comboCode = baseCombo.code ?? baseCombo.key;
			if (event.type === 'keydown') {
				if (baseCombo.key === 'Escape') {
					cancelRecording();
					return;
				}
				if (isGlobalKeyEventModifierKey(event)) {
					heldModifierCodes.add(comboCode);
					seenModifierCodes.add(comboCode);
					lastModifierCombo = {...baseCombo, modifierOnly: true};
					setPreviewCombo(baseCombo);
					return;
				}
				const combo: KeyCombo = {
					...baseCombo,
					...mergeModifiers(baseCombo, reconstructModifiersFromPhysicallyHeldKeys(heldModifierCodes)),
				};
				setPreviewCombo(combo);
				if (!combo.key && !combo.code) return;
				sawNonModifier = true;
				commit(combo);
				return;
			}
			if (!isGlobalKeyEventModifierKey(event)) return;
			if (sawNonModifier) return;
			heldModifierCodes.delete(comboCode);
			if (heldModifierCodes.size > 0) return;
			const combo = lastModifierCombo ?? globalKeyEventToCombo(event, {modifierOnly: true});
			if (!combo) return;
			if (!combo.key && !combo.code) return;
			const modifierFlagCount =
				(combo.shift ? 1 : 0) +
				(combo.ctrl ? 1 : 0) +
				(combo.alt ? 1 : 0) +
				(combo.meta ? 1 : 0) +
				(combo.ctrlOrMeta ? 1 : 0);
			if (modifierFlagCount === 1) {
				for (const [leftCode, rightCode] of MODIFIER_BOTH_SIDES_PAIRS) {
					if (seenModifierCodes.has(leftCode) && seenModifierCodes.has(rightCode)) {
						commit({...combo, code: leftCode, bothSides: true});
						return;
					}
				}
			}
			commit(combo);
		};
		const handleMouseDown = (event: MouseEvent) => {
			if (!RECORDABLE_MOUSE_BUTTONS.has(event.button)) return;
			event.preventDefault();
			event.stopPropagation();
			sawNonModifier = true;
			commit({
				key: '',
				mouseButton: event.button,
				...modifierStateToCombo(event),
			});
		};
		const handleContextMenu = (event: MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
		};
		const baselineButtons = new Map<number, Set<number>>();
		const snapshotGamepad = (pad: Gamepad): Set<number> => {
			const held = new Set<number>();
			for (let i = 0; i < pad.buttons.length; i++) {
				if (isGamepadButtonPressed(pad.buttons[i])) held.add(i);
			}
			return held;
		};
		for (const pad of navigator.getGamepads?.() ?? []) {
			if (pad) baselineButtons.set(pad.index, snapshotGamepad(pad));
		}
		let rafId = 0;
		const pollGamepads = () => {
			const pads = navigator.getGamepads?.() ?? [];
			for (const pad of pads) {
				if (!pad) continue;
				const baseline = baselineButtons.get(pad.index) ?? new Set<number>();
				for (let i = 0; i < pad.buttons.length; i++) {
					const pressed = isGamepadButtonPressed(pad.buttons[i]);
					if (pressed && !baseline.has(i)) {
						commit({key: '', gamepadButton: i});
						return;
					}
					if (!pressed && baseline.has(i)) {
						baseline.delete(i);
					}
				}
				baselineButtons.set(pad.index, baseline);
			}
			rafId = requestAnimationFrame(pollGamepads);
		};
		rafId = requestAnimationFrame(pollGamepads);
		const cancelGlobalCapture = beginGlobalKeyCapture(getElectronAPI(), (event) => {
			handleGlobalKeyEvent(event);
		});
		window.addEventListener('keydown', handleKeyDown, true);
		window.addEventListener('keyup', handleKeyUp, true);
		window.addEventListener('mousedown', handleMouseDown, true);
		window.addEventListener('contextmenu', handleContextMenu, true);
		return () => {
			cancelGlobalCapture();
			window.removeEventListener('keydown', handleKeyDown, true);
			window.removeEventListener('keyup', handleKeyUp, true);
			window.removeEventListener('mousedown', handleMouseDown, true);
			window.removeEventListener('contextmenu', handleContextMenu, true);
			cancelAnimationFrame(rafId);
		};
	}, [recording, onSave, cancelRecording, finishRecording, value.global]);
	const handleClear = () => {
		setPreviewCombo(null);
		onClear?.();
	};
	const handleReset = () => {
		setPreviewCombo(null);
		onReset?.();
	};
	return (
		<div className={styles.popout} data-flx="input.keybind-recorder.keybind-editor-popout.popout">
			<div className={styles.popoutHeader} data-flx="input.keybind-recorder.keybind-editor-popout.popout-header">
				<span className={styles.popoutTitle} data-flx="input.keybind-recorder.keybind-editor-popout.popout-title">
					<Trans>Edit shortcut</Trans>
				</span>
				<span className={styles.popoutHint} data-flx="input.keybind-recorder.keybind-editor-popout.popout-hint">
					<Trans>Press a key, mouse button, or gamepad button to bind. Escape cancels.</Trans>
				</span>
			</div>
			<FocusRing offset={-2} data-flx="input.keybind-recorder.keybind-editor-popout.focus-ring">
				<div
					className={clsx(styles.recorderBox, recording && styles.recorderBoxRecording)}
					onClick={startRecording}
					onKeyDown={(e) => {
						if (isKeyboardActivationKey(e.key)) {
							e.preventDefault();
							startRecording();
						}
					}}
					tabIndex={0}
					role="button"
					aria-label={i18n._(RECORD_SHORTCUT_DESCRIPTOR)}
					data-flx="input.keybind-recorder.keybind-editor-popout.recorder-box.start-recording"
				>
					<KeyboardIcon
						size={remFromPx(20)}
						weight="bold"
						className={styles.recorderIcon}
						data-flx="input.keybind-recorder.keybind-editor-popout.recorder-icon"
					/>
					<span className={styles.recorderText} data-flx="input.keybind-recorder.keybind-editor-popout.recorder-text">
						{recording ? (
							<Trans>Press a shortcut…</Trans>
						) : currentHasValue ? (
							displayValue
						) : (
							<Trans>Click to record</Trans>
						)}
					</span>
				</div>
			</FocusRing>
			{defaultDisplayValue && (
				<div className={styles.defaultRow} data-flx="input.keybind-recorder.keybind-editor-popout.default-row">
					<span className={styles.defaultLabel} data-flx="input.keybind-recorder.keybind-editor-popout.default-label">
						<Trans>Default:</Trans>
					</span>
					<span className={styles.defaultValue} data-flx="input.keybind-recorder.keybind-editor-popout.default-value">
						{defaultDisplayValue}
					</span>
				</div>
			)}
			<div className={styles.popoutActions} data-flx="input.keybind-recorder.keybind-editor-popout.popout-actions">
				<div
					className={styles.popoutActionsLeft}
					data-flx="input.keybind-recorder.keybind-editor-popout.popout-actions-left"
				>
					{onClear && currentHasValue && (
						<Button
							variant="secondary"
							small
							type="button"
							onClick={handleClear}
							leftIcon={
								<TrashIcon size={remFromPx(16)} data-flx="input.keybind-recorder.keybind-editor-popout.trash-icon" />
							}
							data-flx="input.keybind-recorder.keybind-editor-popout.button.clear"
						>
							<Trans>Clear</Trans>
						</Button>
					)}
					{onReset && currentIsModified && (
						<Button
							variant="secondary"
							small
							type="button"
							onClick={handleReset}
							leftIcon={
								<ArrowCounterClockwiseIcon
									size={remFromPx(16)}
									data-flx="input.keybind-recorder.keybind-editor-popout.arrow-counter-clockwise-icon"
								/>
							}
							data-flx="input.keybind-recorder.keybind-editor-popout.button.reset"
						>
							<Trans>Reset</Trans>
						</Button>
					)}
				</div>
				<Button
					variant="secondary"
					small
					type="button"
					onClick={onClose}
					data-flx="input.keybind-recorder.keybind-editor-popout.button.close"
				>
					<Trans>Done</Trans>
				</Button>
			</div>
		</div>
	);
};
export const KeybindRecorder: React.FC<KeybindRecorderProps> = ({
	action: _action,
	label,
	labelPlacement = 'stacked',
	value,
	defaultValue = null,
	disabled = false,
	onChange,
	onClear,
	onReset,
	className,
	'data-flx': dataFlx,
}) => {
	const {i18n} = useLingui();
	const triggerRef = useRef<HTMLButtonElement | null>(null);
	const isEmpty = !value?.key && !value?.code && value?.mouseButton === undefined && value?.gamepadButton === undefined;
	const hasValue = !isEmpty;
	const displayValue = formatKeyCombo(value);
	return (
		<div
			className={clsx(styles.field, labelPlacement === 'inline' && styles.fieldInline)}
			data-flx={dataFlx ?? 'input.keybind-recorder.field'}
		>
			<span className={styles.fieldLabel} data-flx="input.keybind-recorder.field-label">
				{label}
			</span>
			<Popout
				position="bottom"
				offsetMainAxis={8}
				offsetCrossAxis={0}
				returnFocusRef={triggerRef}
				render={({onClose}) => (
					<KeybindEditorPopout
						value={value}
						defaultValue={defaultValue}
						onSave={(combo) => {
							onChange({...combo, global: value.global});
						}}
						onClear={onClear}
						onReset={onReset}
						onClose={onClose}
						data-flx="input.keybind-recorder.keybind-editor-popout"
					/>
				)}
				data-flx="input.keybind-recorder.popout"
			>
				<button
					ref={triggerRef}
					type="button"
					className={clsx(styles.recorder, hasValue && styles.hasValue, disabled && styles.disabled, className)}
					disabled={disabled}
					aria-label={i18n._(EDIT_KEYBOARD_SHORTCUT_DESCRIPTOR)}
					data-flx="input.keybind-recorder.recorder.button"
				>
					<div className={styles.layout} data-flx="input.keybind-recorder.layout">
						<div className={styles.editIconLeft} aria-hidden data-flx="input.keybind-recorder.edit-icon-left">
							<PencilSimpleIcon
								size={remFromPx(12)}
								weight="bold"
								data-flx="input.keybind-recorder.pencil-simple-icon"
							/>
						</div>
						<div className={styles.inputWrapper} data-flx="input.keybind-recorder.input-wrapper">
							<span className={styles.input} data-flx="input.keybind-recorder.input">
								{hasValue ? displayValue : i18n._(NO_SHORTCUT_SET_DESCRIPTOR)}
							</span>
						</div>
					</div>
				</button>
			</Popout>
		</div>
	);
};
