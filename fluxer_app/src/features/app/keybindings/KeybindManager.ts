// SPDX-License-Identifier: AGPL-3.0-or-later

import {organizeChannels} from '@app/features/app/components/layout/utils/ChannelOrganization';
import {
	keyboardEventCanRecoverStaleMacMetaPress,
	keyboardEventMatchesCombo,
	keyboardEventReleasesComboModifier,
	keyboardEventStartsComboPress,
	keyboardEventTriggerMatchesCombo,
	shouldAllowLocalShortcutForChannelTextarea,
} from '@app/features/app/keybindings/KeybindEventUtils';
import {
	isKeybindAllowedDuringVoiceCallFullscreen,
	isKeybindBlockedByCompactVoiceCallView,
} from '@app/features/app/keybindings/KeybindScopeUtils';
import {registerDefaultKeybindHandlers} from '@app/features/app/keybindings/keybind_manager/handlers/defaultHandlers';
import {registerMessageHandlers} from '@app/features/app/keybindings/keybind_manager/handlers/messageHandlers';
import type {
	CombokeysInstance,
	HoldBindingRuntime,
	KeybindHandler,
	ShortcutSource,
} from '@app/features/app/keybindings/keybind_manager/shared';
import {comboToCombokeysStrings} from '@app/features/app/keybindings/utils/ComboShortcutStrings';
import {
	EDITABLE_CAPTURE_SHORTCUT_ACTIONS,
	getEditableElementValue,
	isChannelTextareaElement,
	isEditableElement,
} from '@app/features/app/keybindings/utils/EditableElement';
import {keyNameForGlobalHook, physicalKeyNameForGlobalHook} from '@app/features/app/keybindings/utils/GlobalHookKeys';
import {
	shouldSuppressLocalShortcutForModalFocus,
	shouldSuppressShortcutForFullscreenMedia,
} from '@app/features/app/keybindings/utils/ModalSuppression';
import {
	buildCustomRuntimeKeybinds,
	buildDefaultRuntimeKeybinds,
	getCustomActionOverrides,
	HOLD_ACTIONS,
	HOLD_ACTIONS_FOR_PTT_MODE,
	HOLD_ACTIONS_FOR_VOICE_ACTIVITY_MODE,
	type HoldAction,
	hookShortcutIdForAction,
	hookShortcutIdForKeybind,
	type RuntimeKeybind,
} from '@app/features/app/keybindings/utils/RuntimeKeybinds';
import {LOCAL_SHORTCUT_ACTION_PRIORITY} from '@app/features/app/keybindings/utils/ShortcutPriority';
import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildList from '@app/features/guild/state/GuildList';
import Guilds from '@app/features/guild/state/Guilds';
import Keybind, {
	type CustomKeybindEntry,
	type KeybindCommand,
	type KeybindConfig,
	type KeyCombo,
} from '@app/features/input/state/InputKeybind';
import {isGamepadButtonPressed} from '@app/features/input/utils/GamepadButtonUtils';
import {shouldPreferLayoutKeyForShortcut} from '@app/features/input/utils/KeybindComboUtils';
import {shouldUseKeyboardShortcutsOverlayFallbackFromEvent} from '@app/features/input/utils/KeyboardShortcutLayoutUtils';
import {jsKeyToUiohookKeycode} from '@app/features/input/utils/UiohookKeycodes';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageFocus from '@app/features/messaging/state/MessageFocus';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import NativePermission, {
	type LinuxInputAccessNagbarReason,
} from '@app/features/permissions/system/state/NativePermission';
import {ensureMacPermission} from '@app/features/permissions/system/utils/MacPermissionGate';
import {Logger} from '@app/features/platform/utils/AppLogger';
import ReadStates from '@app/features/read_state/state/ReadStates';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {isWebReservedZoomShortcut} from '@app/features/ui/utils/AppZoomKeybindUtils';
import {getElectronAPI, isNativeMacOS} from '@app/features/ui/utils/NativeUtils';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallInitiator from '@app/features/voice/state/CallInitiator';
import CallState from '@app/features/voice/state/CallState';
import CompactVoiceCallHeight, {
	getCompactVoiceCallExpansionKey,
	getGuildVoiceCallExpansionKey,
} from '@app/features/voice/state/CompactVoiceCallHeight';
import MockIncomingCall from '@app/features/voice/state/MockIncomingCall';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import CombokeysImport from 'combokeys';
import {autorun, reaction} from 'mobx';

const normalizeKeyboardShortcutKey = (key: string): string => {
	if (key === ' ') return 'space';
	if (key === 'Break') return 'pause';
	return key.toLowerCase();
};

export {comboToCombokeysStrings} from '@app/features/app/keybindings/utils/ComboShortcutStrings';
export {keyNameForGlobalHook, physicalKeyNameForGlobalHook} from '@app/features/app/keybindings/utils/GlobalHookKeys';
export {
	shouldSuppressLocalShortcutForModalFocus,
	shouldSuppressShortcutForFullscreenMedia,
} from '@app/features/app/keybindings/utils/ModalSuppression';

const ROUTE_ALLOWED_ACTIONS = new Set<KeybindCommand>(['system_open_theme_studio_popout']);
const GAMEPAD_POLL_INTERVAL_MS = 50;

class KeybindManager {
	private handlers = new Map<KeybindCommand, KeybindHandler>();
	private initialized = false;
	private manualSuspendCount = 0;
	private routeSuspended = false;
	private disposers: Array<() => void> = [];
	private combokeys: CombokeysInstance | null = null;
	private inputMonitoringHookStatus: 'unknown' | 'granted' | 'denied' = 'unknown';
	pttReleaseTimer: NodeJS.Timeout | null = null;
	private registeredGlobalHookShortcutIds = new Set<string>();
	private globalKeyHookUnsubscribes: Array<() => void> = [];
	private globalKeybindTriggeredUnsubscribe: (() => void) | null = null;
	private globalKeyHookStarted = false;
	private activeGlobalShortcutPressIds = new Set<string>();
	private activeLocalShortcutPressIds = new Set<string>();
	private inputSyncQueue: Promise<void> = Promise.resolve();
	private holdBindings: Array<HoldBindingRuntime> = [];
	private localHoldListenerAttached = false;
	private localKeyboardShortcutListenerAttached = false;
	private localMouseShortcutListenerAttached = false;
	private gamepadPollIntervalId: number | null = null;
	private gamepadListenersAttached = false;
	private gamepadShortcutStates = new Map<string, {binding: RuntimeKeybind; pressed: boolean}>();
	logger = new Logger('KeybindManager');

	private get suspended(): boolean {
		return this.manualSuspendCount > 0;
	}

	get currentChannelId(): string | null {
		return SelectedChannel.currentChannelId;
	}

	get currentGuildId(): string | null {
		return SelectedGuild.selectedGuildId;
	}

	navigateToChannel(guildId: string | null, channelId: string): void {
		const channel = Channels.getChannel(channelId);
		const effectiveGuildId = guildId ?? channel?.guildId ?? null;
		if (channel?.guildId) {
			NavigationCommands.selectChannel(channel.guildId, channelId);
			return;
		}
		if (channel && !channel.guildId) {
			NavigationCommands.selectChannel(ME, channelId);
			return;
		}
		if (effectiveGuildId) {
			NavigationCommands.selectChannel(effectiveGuildId, channelId);
		}
	}

	private get activeKeybinds(): Array<RuntimeKeybind> {
		const skipDefaults = Keybind.getDisableBuiltinKeybinds();
		const defaults = skipDefaults ? [] : Keybind.getDefaultsForRuntimeDispatch();
		const customs = Keybind.getCustomKeybinds();
		const overriddenActions = getCustomActionOverrides(customs);
		const activeKeybinds = [
			...buildDefaultRuntimeKeybinds(defaults, overriddenActions),
			...buildCustomRuntimeKeybinds(customs, (action) => Keybind.getDefaultByAction(action)),
		];
		return activeKeybinds.filter((entry) => this.isActionAllowedForCurrentView(entry.action));
	}

	private get activeGlobalKeybinds(): Array<RuntimeKeybind> {
		return this.activeKeybinds.filter(
			(k) =>
				!HOLD_ACTIONS.includes(k.action as HoldAction) &&
				k.allowGlobal &&
				(k.combo.global ?? false) &&
				((k.combo.key ?? '') !== '' || (k.combo.code ?? '') !== ''),
		);
	}

	private get activeMouseShortcutKeybinds(): Array<RuntimeKeybind> {
		return this.activeKeybinds.filter(
			(k) => !HOLD_ACTIONS.includes(k.action as HoldAction) && k.combo.mouseButton != null,
		);
	}

	private get activeGamepadShortcutKeybinds(): Array<RuntimeKeybind> {
		return this.activeKeybinds.filter(
			(k) => !HOLD_ACTIONS.includes(k.action as HoldAction) && k.combo.gamepadButton != null,
		);
	}

	private getOrderedGuilds(): Array<Guild> {
		if (GuildList.guilds.length > 0) {
			return GuildList.guilds;
		}
		return Guilds.getGuilds();
	}

	navigateToDirectMessages(): void {
		const channelId = SelectedChannel.selectedChannelIds.get(ME);
		if (channelId && Channels.getChannel(channelId)) {
			this.navigateToChannel(ME, channelId);
			return;
		}
		NavigationCommands.deselectGuild();
	}

	navigateToLastCommunityChannel(): boolean {
		const guildId = this.currentGuildId ?? SelectedGuild.lastSelectedGuildId;
		if (!guildId) return false;
		const channelId =
			SelectedChannel.getNavigableSelectedChannelId(guildId) ?? this.getFirstSelectableChannelId(guildId);
		if (!channelId) return false;
		this.navigateToChannel(guildId, channelId);
		return true;
	}

	navigateToGuildLikeSlot(slotIndex: number): void {
		if (slotIndex === 0) {
			this.navigateToDirectMessages();
			return;
		}
		const guild = this.getOrderedGuilds()[slotIndex - 1];
		if (!guild) return;
		const channelId =
			SelectedChannel.getNavigableSelectedChannelId(guild.id) ?? this.getFirstSelectableChannelId(guild.id);
		if (!channelId) return;
		this.navigateToChannel(guild.id, channelId);
	}

	cycleGuildLikeSlot(direction: 1 | -1): void {
		const guilds = this.getOrderedGuilds();
		const currentGuildId = this.currentGuildId;
		const currentIndex = currentGuildId ? guilds.findIndex((guild) => guild.id === currentGuildId) + 1 : 0;
		const safeIndex = currentIndex <= 0 && currentGuildId ? 0 : currentIndex;
		const slotCount = guilds.length + 1;
		const nextIndex = (safeIndex + direction + slotCount) % slotCount;
		this.navigateToGuildLikeSlot(nextIndex);
	}

	private getFirstSelectableChannelId(guildId: string): string | undefined {
		const channels = Channels.getGuildChannels(guildId);
		const selectableChannel = channels.find((c) => this.isNavigableChannel(c));
		return selectableChannel?.id;
	}

	private isNavigableChannel(channel: Channel): boolean {
		if (channel.type === ChannelTypes.GUILD_CATEGORY) return false;
		if (channel.type === ChannelTypes.GUILD_LINK) return false;
		return true;
	}

	private getNavigableChannelsInCurrentContext(): ReadonlyArray<Channel> {
		const guildId = this.currentGuildId;
		if (!guildId) return [];
		return this.flattenGuildChannelsByDisplayOrder(Channels.getGuildChannels(guildId));
	}

	private flattenGuildChannelsByDisplayOrder(channels: ReadonlyArray<Channel>): Array<Channel> {
		const groups = organizeChannels(channels);
		const flat: Array<Channel> = [];
		for (const group of groups) {
			for (const ch of group.textChannels) {
				if (this.isNavigableChannel(ch)) flat.push(ch);
			}
			for (const ch of group.voiceChannels) {
				if (this.isNavigableChannel(ch)) flat.push(ch);
			}
		}
		return flat;
	}

	private cycleDirectMessageContext(direction: 1 | -1): void {
		const dmChannels = Channels.dmChannels;
		const slotCount = dmChannels.length + 1;
		const currentChannelId = this.currentChannelId;
		const currentIndex = currentChannelId ? dmChannels.findIndex((channel) => channel.id === currentChannelId) + 1 : 0;
		const safeIndex = currentIndex <= 0 && currentChannelId ? 0 : currentIndex;
		const nextIndex = (safeIndex + direction + slotCount) % slotCount;
		if (nextIndex === 0) {
			NavigationCommands.deselectGuild();
			return;
		}
		const channel = dmChannels[nextIndex - 1];
		if (channel) {
			this.navigateToChannel(ME, channel.id);
		}
	}

	cycleChannelInCurrentContext(direction: 1 | -1): void {
		if (Navigation.context === 'dm') {
			this.cycleDirectMessageContext(direction);
			return;
		}
		const channels = this.getNavigableChannelsInCurrentContext();
		if (!channels.length) return;
		const current = this.currentChannelId;
		const idx = current ? channels.findIndex((c) => c.id === current) : -1;
		const base = idx === -1 ? (direction === 1 ? -1 : 0) : idx;
		const target = channels[(base + direction + channels.length) % channels.length];
		this.navigateToChannel(target.guildId ?? null, target.id);
	}

	cycleFilteredChannelInCurrentGuild(predicate: (channel: Channel) => boolean, direction: 1 | -1): void {
		const guildId = this.currentGuildId;
		if (!guildId) return;
		const channels = this.flattenGuildChannelsByDisplayOrder(Channels.getGuildChannels(guildId)).filter((c) =>
			predicate(c),
		);
		if (!channels.length) return;
		const current = this.currentChannelId;
		const idx = current ? channels.findIndex((c) => c.id === current) : -1;
		const base = idx === -1 ? (direction === 1 ? -1 : 0) : idx;
		const target = channels[(base + direction + channels.length) % channels.length];
		this.navigateToChannel(guildId, target.id);
	}

	getIncomingCallChannelId(): string | null {
		const mockCall = MockIncomingCall.mockCall;
		if (mockCall) return mockCall.channel.id;
		const currentUserId = Authentication.currentUserId;
		if (!currentUserId) return null;
		for (const call of CallState.getActiveCalls()) {
			if (MediaEngine.connected && MediaEngine.channelId === call.channelId) continue;
			if (CallInitiator.hasInitiated(call.channelId)) continue;
			if (CallState.isUserPendingRinging(call.channelId, currentUserId)) {
				return call.channelId;
			}
		}
		return null;
	}

	acceptIncomingCall(channelId: string): void {
		if (MockIncomingCall.isMockCall(channelId)) {
			MockIncomingCall.clearMockCall();
			return;
		}
		CallCommands.joinCall(channelId);
	}

	declineIncomingCall(channelId: string): void {
		if (MockIncomingCall.isMockCall(channelId)) {
			MockIncomingCall.clearMockCall();
			return;
		}
		CallCommands.rejectCall(channelId);
	}

	private isActionAvailableForLocalShortcut(action: KeybindCommand): boolean {
		if (action === 'voice_answer_call' || action === 'voice_decline_call') {
			return this.getIncomingCallChannelId() !== null;
		}
		if (action === 'chat_mark_channel_read') {
			const channelId = this.currentChannelId;
			return Boolean(channelId && ReadStates.hasUnread(channelId));
		}
		return true;
	}

	private isAppRoute(pathname: string): boolean {
		return pathname.startsWith('/channels/');
	}

	private ensureCombokeys(): CombokeysInstance | null {
		if (!this.combokeys && CombokeysImport) {
			this.combokeys = new CombokeysImport(document.documentElement);
			if (this.combokeys) {
				this.combokeys.stopCallback = () => false;
			}
		}
		return this.combokeys;
	}

	private async checkInputMonitoringPermission(): Promise<boolean> {
		if (!isNativeMacOS()) return true;
		if (this.inputMonitoringHookStatus === 'granted') return true;
		const result = await ensureMacPermission('input-monitoring', {behavior: 'passive'});
		switch (result) {
			case 'granted':
				NativePermission.setInputMonitoringStatus('granted');
				this.inputMonitoringHookStatus = 'granted';
				return true;
			case 'denied':
			case 'declined':
				NativePermission.setInputMonitoringStatus('denied');
				this.inputMonitoringHookStatus = 'denied';
				return false;
			case 'unsupported-platform':
				this.inputMonitoringHookStatus = 'unknown';
				return true;
		}
	}

	async init(i18n: I18n) {
		if (this.initialized) return;
		this.initialized = true;
		this.ensureCombokeys();
		this.registerDefaultHandlers(i18n);
		await Keybind.refreshKeyboardShortcutLayout();
		this.routeSuspended = !this.isAppRoute(Navigation.pathname);
		this.refreshLocalShortcuts();
		this.disposers.push(
			autorun(() => {
				this.refreshLocalShortcuts();
			}),
		);
		this.disposers.push(
			autorun(() => {
				const desired = this.computeDesiredGlobalHookShortcuts();
				void this.enqueueInputSync(() => this.applyGlobalShortcuts(desired));
			}),
		);
		this.disposers.push(
			reaction(
				() => Keybind.transmitMode,
				() => {
					MediaEngine.handlePushToTalkModeChange();
				},
			),
		);
		this.disposers.push(
			autorun(() => {
				const bindings = this.buildHoldBindings();
				void this.enqueueInputSync(() => this.applyHoldBindings(bindings));
			}),
		);
		this.disposers.push(
			autorun(() => {
				const pathname = Navigation.pathname;
				const isAppRoute = this.isAppRoute(pathname);
				this.setRouteSuspended(!isAppRoute);
			}),
		);
		await this.inputSyncQueue;
	}

	private enqueueInputSync(task: () => Promise<void> | void): Promise<void> {
		const run = this.inputSyncQueue.then(() => task());
		this.inputSyncQueue = run.then(
			() => undefined,
			(error) => {
				this.logger.error('Input binding sync task failed', error);
			},
		);
		return this.inputSyncQueue;
	}

	private buildHoldBindings(): Array<HoldBindingRuntime> {
		const bindings: Array<HoldBindingRuntime> = [];
		const inPttMode = Keybind.isPushToTalkEffective();
		const eligibleActions = inPttMode ? HOLD_ACTIONS_FOR_PTT_MODE : HOLD_ACTIONS_FOR_VOICE_ACTIVITY_MODE;
		const customs: ReadonlyArray<CustomKeybindEntry> = Keybind.getCustomKeybinds();
		const pushBinding = (action: HoldAction, combo: KeyCombo): void => {
			if (!this.isActionAllowedForCurrentView(action)) return;
			const hasBinding = !!(combo.key || combo.code || combo.gamepadButton != null || combo.mouseButton != null);
			if (!hasBinding) return;
			bindings.push({
				action,
				combo,
				keycode: null,
				keyName: null,
				physicalKeyName: null,
				mouseButton: combo.mouseButton ?? null,
				gamepadButton: combo.gamepadButton ?? null,
				isModifierOnly: Boolean(combo.modifierOnly),
				ctrlOrMeta: Boolean(combo.ctrlOrMeta),
				requireBothSides: Boolean(combo.modifierOnly && combo.bothSides),
				modifiers: {
					ctrl: Boolean(combo.ctrl),
					alt: Boolean(combo.alt),
					shift: Boolean(combo.shift),
					meta: Boolean(combo.meta),
				},
				routing: null,
				pressedKeycodes: new Set<number>(),
				localPressedCodes: new Set<string>(),
				localActiveCode: null,
				localMouseActive: false,
				globalMouseActive: false,
				localKeyDown: null,
				localKeyUp: null,
				localMouseDown: null,
				localMouseUp: null,
				gamepadHeld: false,
			});
		};
		for (const action of eligibleActions) {
			for (const entry of customs) {
				if (!entry.enabled) continue;
				if (entry.action !== action) continue;
				pushBinding(action, entry.combo);
			}
		}
		return bindings;
	}

	private async applyHoldBindings(bindings: Array<HoldBindingRuntime>): Promise<void> {
		this.detachLocalHoldListener();
		this.releaseGlobalHoldBindings();
		this.releaseGamepadHoldBindings();
		this.holdBindings = bindings;
		if (bindings.length === 0 || this.suspended || !this.initialized) {
			this.maybeStopGlobalKeyHook();
			this.refreshGamepadPolling();
			return;
		}
		const electronApi = getElectronAPI();
		const globalHookAvailable = !!electronApi?.globalKeyHookStart;
		const wantsGlobal = bindings.some((b) => {
			const hasGlobalRoutable = !!(b.combo.key || b.combo.code || b.mouseButton != null);
			return hasGlobalRoutable && (b.combo.global ?? false) && globalHookAvailable;
		});
		let globalReady = false;
		if (wantsGlobal) {
			globalReady = await this.startGlobalKeyHook('push-to-talk');
		}
		let needsLocal = false;
		for (const binding of bindings) {
			const hasGlobalRoutable = !!(binding.combo.key || binding.combo.code || binding.mouseButton != null);
			if (globalReady && hasGlobalRoutable && (binding.combo.global ?? false)) {
				binding.keycode = jsKeyToUiohookKeycode(binding.combo.code ?? binding.combo.key);
				binding.keyName = keyNameForGlobalHook(binding.combo);
				binding.physicalKeyName = physicalKeyNameForGlobalHook(binding.combo);
				binding.routing = 'global';
			} else if (hasGlobalRoutable || binding.isModifierOnly) {
				binding.routing = 'local';
				needsLocal = true;
			} else {
				binding.routing = null;
			}
		}
		this.maybeStopGlobalKeyHook();
		if (needsLocal) {
			this.attachLocalHoldListener();
		}
		this.refreshGamepadPolling();
	}

	private attachLocalHoldListener(): void {
		if (this.localHoldListenerAttached) {
			this.detachLocalHoldListener();
		}
		this.localHoldListenerAttached = true;
		const onKeyDown = (event: KeyboardEvent): void => {
			for (const binding of this.holdBindings) {
				if (binding.routing !== 'local') continue;
				if (binding.isModifierOnly) {
					if (!this.localKeyEventMatchesModifierOnly(binding, event)) continue;
					if (binding.localPressedCodes.has(event.code)) continue;
					binding.localPressedCodes.add(event.code);
					const required = this.requiredModifierKeyCount(binding);
					if (binding.localPressedCodes.size === required) {
						this.fireHoldHandler(binding, 'press', 'local');
					}
					continue;
				}
				if (!this.localKeyEventMatchesBinding(binding, event)) continue;
				if (binding.localActiveCode === event.code) continue;
				binding.localActiveCode = event.code;
				this.fireHoldHandler(binding, 'press', 'local');
			}
		};
		const onKeyUp = (event: KeyboardEvent): void => {
			for (const binding of this.holdBindings) {
				if (binding.routing !== 'local') continue;
				if (binding.isModifierOnly) {
					if (!binding.localPressedCodes.has(event.code)) continue;
					const required = this.requiredModifierKeyCount(binding);
					const wasAtThreshold = binding.localPressedCodes.size === required;
					binding.localPressedCodes.delete(event.code);
					if (wasAtThreshold) {
						this.fireHoldHandler(binding, 'release', 'local');
					}
					continue;
				}
				if (binding.localActiveCode !== event.code) continue;
				binding.localActiveCode = null;
				this.fireHoldHandler(binding, 'release', 'local');
			}
		};
		const onMouseDown = (event: MouseEvent): void => {
			for (const binding of this.holdBindings) {
				if (binding.routing !== 'local') continue;
				if (binding.mouseButton === null) continue;
				if (event.button !== binding.mouseButton) continue;
				if (!this.matchesModifiers(binding, event)) continue;
				if (binding.localMouseActive) continue;
				binding.localMouseActive = true;
				this.fireHoldHandler(binding, 'press', 'local');
			}
		};
		const onMouseUp = (event: MouseEvent): void => {
			for (const binding of this.holdBindings) {
				if (binding.routing !== 'local') continue;
				if (binding.mouseButton === null) continue;
				if (event.button !== binding.mouseButton) continue;
				if (!binding.localMouseActive) continue;
				binding.localMouseActive = false;
				this.fireHoldHandler(binding, 'release', 'local');
			}
		};
		for (const binding of this.holdBindings) {
			binding.localKeyDown = onKeyDown;
			binding.localKeyUp = onKeyUp;
			binding.localMouseDown = onMouseDown;
			binding.localMouseUp = onMouseUp;
		}
		const onWindowBlur = (): void => {
			for (const binding of this.holdBindings) {
				if (binding.routing !== 'local') continue;
				if (binding.localActiveCode !== null) {
					binding.localActiveCode = null;
					this.fireHoldHandler(binding, 'release', 'local');
				}
				if (binding.localPressedCodes.size > 0) {
					const required = this.requiredModifierKeyCount(binding);
					const wasAtThreshold = binding.localPressedCodes.size >= required;
					binding.localPressedCodes.clear();
					if (wasAtThreshold) {
						this.fireHoldHandler(binding, 'release', 'local');
					}
				}
				if (binding.localMouseActive) {
					binding.localMouseActive = false;
					this.fireHoldHandler(binding, 'release', 'local');
				}
			}
		};
		window.addEventListener('keydown', onKeyDown, true);
		window.addEventListener('keyup', onKeyUp, true);
		window.addEventListener('mousedown', onMouseDown, true);
		document.addEventListener('mouseup', onMouseUp, true);
		window.addEventListener('blur', onWindowBlur);
		this.localKeyDownRef = onKeyDown;
		this.localKeyUpRef = onKeyUp;
		this.localMouseDownRef = onMouseDown;
		this.localMouseUpRef = onMouseUp;
		this.localBlurRef = onWindowBlur;
	}

	private localKeyDownRef: ((event: KeyboardEvent) => void) | null = null;
	private localKeyUpRef: ((event: KeyboardEvent) => void) | null = null;
	private localMouseDownRef: ((event: MouseEvent) => void) | null = null;
	private localMouseUpRef: ((event: MouseEvent) => void) | null = null;
	private localBlurRef: (() => void) | null = null;
	private localEditableShortcutKeyDownRef: ((event: KeyboardEvent) => void) | null = null;
	private localEditableShortcutKeyUpRef: ((event: KeyboardEvent) => void) | null = null;
	private localShortcutKeyDownRef: ((event: KeyboardEvent) => void) | null = null;
	private localShortcutKeyUpRef: ((event: KeyboardEvent) => void) | null = null;
	private localShortcutMouseDownRef: ((event: MouseEvent) => void) | null = null;
	private localShortcutMouseUpRef: ((event: MouseEvent) => void) | null = null;

	private detachLocalHoldListener(): void {
		if (!this.localHoldListenerAttached) return;
		this.localHoldListenerAttached = false;
		if (this.localKeyDownRef) window.removeEventListener('keydown', this.localKeyDownRef, true);
		if (this.localKeyUpRef) window.removeEventListener('keyup', this.localKeyUpRef, true);
		if (this.localMouseDownRef) window.removeEventListener('mousedown', this.localMouseDownRef, true);
		if (this.localMouseUpRef) document.removeEventListener('mouseup', this.localMouseUpRef, true);
		if (this.localBlurRef) window.removeEventListener('blur', this.localBlurRef);
		this.localKeyDownRef = null;
		this.localKeyUpRef = null;
		this.localMouseDownRef = null;
		this.localMouseUpRef = null;
		this.localBlurRef = null;
		for (const binding of this.holdBindings) {
			if (binding.localActiveCode !== null) {
				binding.localActiveCode = null;
				this.fireHoldHandler(binding, 'release', 'local');
			}
			if (binding.localPressedCodes.size > 0) {
				const required = this.requiredModifierKeyCount(binding);
				const wasAtThreshold = binding.localPressedCodes.size >= required;
				binding.localPressedCodes.clear();
				if (wasAtThreshold) {
					this.fireHoldHandler(binding, 'release', 'local');
				}
			}
			if (binding.localMouseActive) {
				binding.localMouseActive = false;
				this.fireHoldHandler(binding, 'release', 'local');
			}
			binding.localKeyDown = null;
			binding.localKeyUp = null;
			binding.localMouseDown = null;
			binding.localMouseUp = null;
		}
	}

	private refreshLocalMouseShortcutListener(): void {
		this.detachLocalMouseShortcutListener();
		if (this.activeMouseShortcutKeybinds.length === 0) return;
		this.localMouseShortcutListenerAttached = true;
		const onMouseDown = (event: MouseEvent): void => {
			this.handleLocalMouseShortcutEvent(event, 'press');
		};
		const onMouseUp = (event: MouseEvent): void => {
			this.handleLocalMouseShortcutEvent(event, 'release');
		};
		window.addEventListener('mousedown', onMouseDown, true);
		document.addEventListener('mouseup', onMouseUp, true);
		this.localShortcutMouseDownRef = onMouseDown;
		this.localShortcutMouseUpRef = onMouseUp;
	}

	private detachLocalMouseShortcutListener(): void {
		if (!this.localMouseShortcutListenerAttached) return;
		this.localMouseShortcutListenerAttached = false;
		if (this.localShortcutMouseDownRef) window.removeEventListener('mousedown', this.localShortcutMouseDownRef, true);
		if (this.localShortcutMouseUpRef) document.removeEventListener('mouseup', this.localShortcutMouseUpRef, true);
		this.localShortcutMouseDownRef = null;
		this.localShortcutMouseUpRef = null;
	}

	private refreshLocalKeyboardShortcutListener(): void {
		this.detachLocalKeyboardShortcutListener();
		if (this.activeKeybinds.every((entry) => !this.canBindLocalShortcut(entry))) return;
		this.localKeyboardShortcutListenerAttached = true;
		const onKeyDown = (event: KeyboardEvent): void => {
			this.handleLocalKeyboardShortcutEvent(event, 'press');
		};
		const onKeyUp = (event: KeyboardEvent): void => {
			this.handleLocalKeyboardShortcutEvent(event, 'release');
		};
		window.addEventListener('keydown', onKeyDown, true);
		window.addEventListener('keyup', onKeyUp, true);
		this.localShortcutKeyDownRef = onKeyDown;
		this.localShortcutKeyUpRef = onKeyUp;
	}

	private detachLocalKeyboardShortcutListener(): void {
		if (!this.localKeyboardShortcutListenerAttached) return;
		this.localKeyboardShortcutListenerAttached = false;
		if (this.localShortcutKeyDownRef) window.removeEventListener('keydown', this.localShortcutKeyDownRef, true);
		if (this.localShortcutKeyUpRef) window.removeEventListener('keyup', this.localShortcutKeyUpRef, true);
		this.localShortcutKeyDownRef = null;
		this.localShortcutKeyUpRef = null;
	}

	private refreshLocalEditableShortcutCaptureListener(): void {
		this.detachLocalEditableShortcutCaptureListener();
		if (this.activeKeybinds.every((entry) => !this.shouldCaptureLocalShortcutInEditable(entry))) return;
		const onKeyDown = (event: KeyboardEvent): void => {
			this.handleLocalEditableShortcutCaptureEvent(event, 'press');
		};
		const onKeyUp = (event: KeyboardEvent): void => {
			this.handleLocalEditableShortcutCaptureEvent(event, 'release');
		};
		window.addEventListener('keydown', onKeyDown, true);
		window.addEventListener('keyup', onKeyUp, true);
		this.localEditableShortcutKeyDownRef = onKeyDown;
		this.localEditableShortcutKeyUpRef = onKeyUp;
	}

	private detachLocalEditableShortcutCaptureListener(): void {
		if (this.localEditableShortcutKeyDownRef) {
			window.removeEventListener('keydown', this.localEditableShortcutKeyDownRef, true);
		}
		if (this.localEditableShortcutKeyUpRef) {
			window.removeEventListener('keyup', this.localEditableShortcutKeyUpRef, true);
		}
		this.localEditableShortcutKeyDownRef = null;
		this.localEditableShortcutKeyUpRef = null;
	}

	private shouldCaptureLocalShortcutInEditable(entry: RuntimeKeybind): boolean {
		if (!EDITABLE_CAPTURE_SHORTCUT_ACTIONS.has(entry.action)) return false;
		if (!this.canBindLocalShortcut(entry)) return false;
		return entry.editableFocusBehavior === 'allow';
	}

	private handleLocalEditableShortcutCaptureEvent(event: KeyboardEvent, type: 'press' | 'release'): void {
		if (event.defaultPrevented) return;
		const target = event.target ?? null;
		if (!isEditableElement(target) || !isChannelTextareaElement(target)) return;
		const entries = this.orderLocalShortcutGroup(
			this.activeKeybinds.filter((entry) => this.shouldCaptureLocalShortcutInEditable(entry)),
		);
		for (const entry of entries) {
			if (!keyboardEventMatchesCombo(entry.combo, event, {isMacOS: isNativeMacOS()})) continue;
			if (!this.dispatchLocalShortcut(entry, type, event)) continue;
			event.stopPropagation();
			event.stopImmediatePropagation();
			return;
		}
	}

	private handleLocalMouseShortcutEvent(event: MouseEvent, type: 'press' | 'release'): void {
		for (const binding of this.activeMouseShortcutKeybinds) {
			const combo = binding.combo;
			if (combo.mouseButton == null) continue;
			if (event.button !== combo.mouseButton) continue;
			if (type === 'press' && shouldSuppressLocalShortcutForModalFocus(binding, event.target ?? null)) continue;
			const id = hookShortcutIdForKeybind(binding);
			if (!id) continue;
			const isRegisteredGlobalShortcut = this.isHookShortcutRegistered(binding);
			if (type === 'release') {
				if (!this.activeLocalShortcutPressIds.delete(id)) continue;
				if (isRegisteredGlobalShortcut) {
					this.activeGlobalShortcutPressIds.delete(id);
				}
				this.fireShortcutHandler(binding, type, 'local', {shiftKey: event.shiftKey});
				continue;
			}
			if (!this.comboModifiersMatch(combo, event)) continue;
			if (this.activeLocalShortcutPressIds.has(id)) continue;
			if (isRegisteredGlobalShortcut) {
				if (this.activeGlobalShortcutPressIds.has(id)) continue;
				this.activeGlobalShortcutPressIds.add(id);
			}
			this.activeLocalShortcutPressIds.add(id);
			this.fireShortcutHandler(binding, type, 'local', {shiftKey: event.shiftKey});
		}
	}

	private localKeyboardShortcutId(entry: RuntimeKeybind): string {
		const combo = entry.combo;
		return JSON.stringify({
			key: combo.key || '',
			code: combo.code || '',
			ctrlOrMeta: Boolean(combo.ctrlOrMeta),
			ctrl: Boolean(combo.ctrl),
			alt: Boolean(combo.alt),
			shift: Boolean(combo.shift),
			meta: Boolean(combo.meta),
		});
	}

	private clearActiveLocalKeyboardShortcut(entry: RuntimeKeybind, id: string): void {
		this.activeLocalShortcutPressIds.delete(id);
		for (const shortcut of comboToCombokeysStrings(entry.combo)) {
			this.activeLocalShortcutPressIds.delete(shortcut);
		}
		const registeredHookShortcutId = hookShortcutIdForKeybind(entry);
		if (
			(entry.combo.global ?? false) &&
			registeredHookShortcutId &&
			this.registeredGlobalHookShortcutIds.has(registeredHookShortcutId)
		) {
			this.activeGlobalShortcutPressIds.delete(registeredHookShortcutId);
		}
	}

	private handleLocalKeyboardShortcutEvent(event: KeyboardEvent, type: 'press' | 'release'): void {
		if (event.defaultPrevented) return;
		const entries = this.orderLocalShortcutGroup(
			this.activeKeybinds.filter((entry) => this.canBindLocalShortcut(entry)),
		);
		const isMacOS = isNativeMacOS();
		for (const entry of entries) {
			const id = this.localKeyboardShortcutId(entry);
			if (type === 'release') {
				if (!this.activeLocalShortcutPressIds.has(id)) continue;
				if (!keyboardEventTriggerMatchesCombo(entry.combo, event)) {
					if (keyboardEventReleasesComboModifier(entry.combo, event, {isMacOS})) {
						this.clearActiveLocalKeyboardShortcut(entry, id);
					}
					continue;
				}
				this.clearActiveLocalKeyboardShortcut(entry, id);
				if (this.dispatchLocalShortcut(entry, type, event)) {
					event.stopPropagation();
					event.stopImmediatePropagation();
				}
				return;
			}
			if (!keyboardEventStartsComboPress(entry.combo, event, {isMacOS})) continue;
			if (this.activeLocalShortcutPressIds.has(id)) {
				if (
					this.isHookShortcutRegistered(entry) ||
					!keyboardEventCanRecoverStaleMacMetaPress(entry.combo, event, {isMacOS})
				) {
					continue;
				}
				this.clearActiveLocalKeyboardShortcut(entry, id);
			}
			if (this.dispatchLocalShortcut(entry, type, event)) {
				this.activeLocalShortcutPressIds.add(id);
				event.stopPropagation();
				event.stopImmediatePropagation();
				return;
			}
		}
	}

	private localKeyEventMatchesBinding(binding: HoldBindingRuntime, event: KeyboardEvent): boolean {
		const expectedCode = binding.combo.code ?? null;
		const expectedKey = binding.combo.key ?? null;
		if (shouldPreferLayoutKeyForShortcut(binding.combo) && expectedKey) {
			if (normalizeKeyboardShortcutKey(event.key) !== normalizeKeyboardShortcutKey(expectedKey)) return false;
		} else if (expectedCode && event.code !== expectedCode) {
			return false;
		} else if (!expectedCode && expectedKey && event.key !== expectedKey) {
			return false;
		}
		if (!expectedCode && !expectedKey) return false;
		if (binding.isModifierOnly) return true;
		return this.matchesModifiers(binding, event);
	}

	private requiredModifierKeyCount(binding: HoldBindingRuntime): number {
		if (binding.requireBothSides) return 2;
		const {modifiers, ctrlOrMeta} = binding;
		let count = 0;
		if (ctrlOrMeta) count++;
		if (modifiers.ctrl) count++;
		if (modifiers.alt) count++;
		if (modifiers.shift) count++;
		if (modifiers.meta) count++;
		return Math.max(count, 1);
	}

	private localKeyEventMatchesModifierOnly(binding: HoldBindingRuntime, event: KeyboardEvent): boolean {
		const {modifiers, ctrlOrMeta} = binding;
		const eventKey = event.key;
		if (ctrlOrMeta) {
			if (isNativeMacOS()) return eventKey === 'Meta';
			return eventKey === 'Control';
		}
		if (modifiers.ctrl && eventKey === 'Control') return true;
		if (modifiers.alt && eventKey === 'Alt') return true;
		if (modifiers.shift && eventKey === 'Shift') return true;
		if (modifiers.meta && eventKey === 'Meta') return true;
		return false;
	}

	private matchesModifiers(binding: HoldBindingRuntime, event: KeyboardEvent | MouseEvent): boolean {
		return this.comboModifiersMatch(binding.combo, event);
	}

	private comboModifiersMatch(combo: KeyCombo, event: KeyboardEvent | MouseEvent): boolean {
		const anyModifier = Boolean(combo.ctrlOrMeta || combo.ctrl || combo.meta || combo.alt || combo.shift);
		if (!anyModifier) return true;
		if (combo.ctrlOrMeta && (isNativeMacOS() ? !event.metaKey : !event.ctrlKey)) return false;
		if (combo.ctrl !== undefined && event.ctrlKey !== Boolean(combo.ctrl)) return false;
		if (combo.meta !== undefined && event.metaKey !== Boolean(combo.meta)) return false;
		if (combo.alt !== undefined && event.altKey !== Boolean(combo.alt)) return false;
		if (combo.shift !== undefined && event.shiftKey !== Boolean(combo.shift)) return false;
		return true;
	}

	private hasGamepadBindings(): boolean {
		return (
			this.holdBindings.some((binding) => binding.gamepadButton !== null) ||
			this.activeGamepadShortcutKeybinds.length > 0
		);
	}

	private refreshGamepadPolling(): void {
		if (!this.hasGamepadBindings() || typeof navigator.getGamepads !== 'function') {
			this.stopGamepadMonitoring();
			return;
		}
		this.ensureGamepadListeners();
		if (this.hasConnectedGamepad()) {
			this.startGamepadPolling();
			return;
		}
		this.stopGamepadPolling();
	}

	private ensureGamepadListeners(): void {
		if (!this.gamepadListenersAttached) {
			this.gamepadListenersAttached = true;
			window.addEventListener('gamepadconnected', this.handleGamepadConnected);
			window.addEventListener('gamepaddisconnected', this.handleGamepadDisconnected);
		}
	}

	private startGamepadPolling(): void {
		if (this.gamepadPollIntervalId !== null) return;
		if (typeof navigator.getGamepads !== 'function') return;
		this.tickGamepadBindings();
		this.gamepadPollIntervalId = window.setInterval(() => this.tickGamepadBindings(), GAMEPAD_POLL_INTERVAL_MS);
	}

	private stopGamepadPolling(): void {
		if (this.gamepadPollIntervalId !== null) {
			window.clearInterval(this.gamepadPollIntervalId);
			this.gamepadPollIntervalId = null;
		}
		this.releaseGamepadShortcutStates();
		this.releaseGamepadHoldBindings();
	}

	private stopGamepadMonitoring(): void {
		this.stopGamepadPolling();
		if (this.gamepadListenersAttached) {
			this.gamepadListenersAttached = false;
			window.removeEventListener('gamepadconnected', this.handleGamepadConnected);
			window.removeEventListener('gamepaddisconnected', this.handleGamepadDisconnected);
		}
	}

	private releaseGamepadHoldBindings(): void {
		for (const binding of this.holdBindings) {
			if (binding.gamepadHeld) {
				binding.gamepadHeld = false;
				this.fireHoldHandler(binding, 'release', 'local');
			}
		}
	}

	private releaseGamepadShortcutStates(): void {
		for (const {binding, pressed} of this.gamepadShortcutStates.values()) {
			if (pressed) {
				this.fireShortcutHandler(binding, 'release', 'local');
			}
		}
		this.gamepadShortcutStates.clear();
	}

	private handleGamepadConnected = (): void => {
		for (const binding of this.holdBindings) binding.gamepadHeld = false;
		this.gamepadShortcutStates.clear();
		if (this.hasGamepadBindings()) {
			this.startGamepadPolling();
		}
	};
	private handleGamepadDisconnected = (): void => {
		if (this.hasConnectedGamepad()) {
			this.tickGamepadBindings();
			return;
		}
		this.stopGamepadPolling();
	};

	private hasConnectedGamepad(): boolean {
		const pads = navigator.getGamepads?.() ?? [];
		for (const pad of pads) {
			if (pad != null) return true;
		}
		return false;
	}

	private tickGamepadBindings(): void {
		const pads = navigator.getGamepads?.() ?? [];
		for (const binding of this.holdBindings) {
			const target = binding.gamepadButton;
			if (target === null) continue;
			const pressed = this.isGamepadButtonPressed(pads, target);
			if (pressed === binding.gamepadHeld) continue;
			binding.gamepadHeld = pressed;
			this.fireHoldHandler(binding, pressed ? 'press' : 'release', 'local');
		}
		for (const binding of this.activeGamepadShortcutKeybinds) {
			const target = binding.combo.gamepadButton;
			if (target == null) continue;
			const id = hookShortcutIdForKeybind(binding) ?? `gamepad:${binding.action}:${target}`;
			const pressed = this.isGamepadButtonPressed(pads, target);
			const previous = this.gamepadShortcutStates.get(id)?.pressed ?? false;
			if (pressed === previous) continue;
			this.gamepadShortcutStates.set(id, {binding, pressed});
			this.fireShortcutHandler(binding, pressed ? 'press' : 'release', 'local');
		}
	}

	private isGamepadButtonPressed(pads: ReadonlyArray<Gamepad | null>, target: number): boolean {
		for (const pad of pads) {
			if (!pad) continue;
			const btn = pad.buttons[target];
			if (isGamepadButtonPressed(btn)) return true;
		}
		return false;
	}

	private fireHoldHandler(binding: HoldBindingRuntime, type: 'press' | 'release', source: ShortcutSource): void {
		const handler = this.handlers.get(binding.action);
		if (!handler) return;
		if (type === 'press') {
			if (this.suspended) return;
			if (!this.isActionAllowedForCurrentView(binding.action)) return;
			if (shouldSuppressShortcutForFullscreenMedia()) return;
			if (Keybind.isActionMuted(binding.action)) return;
		}
		handler({type, source});
	}

	private fireShortcutHandler(
		binding: RuntimeKeybind,
		type: 'press' | 'release',
		source: ShortcutSource,
		options: {shiftKey?: boolean} = {},
	): void {
		if (this.suspended) return;
		if (!this.isActionAllowedForCurrentView(binding.action)) return;
		if (type === 'press' && shouldSuppressShortcutForFullscreenMedia()) return;
		if (Keybind.isActionMuted(binding.action)) return;
		const handler = this.handlers.get(binding.action);
		if (!handler) return;
		handler({type, source, shiftKey: options.shiftKey});
	}

	async reapplyGlobalShortcuts() {
		if (!this.initialized) return;
		await this.enqueueInputSync(() => this.applyGlobalShortcuts(this.computeDesiredGlobalHookShortcuts()));
		await this.enqueueInputSync(() => this.applyHoldBindings(this.buildHoldBindings()));
	}

	destroy() {
		if (!this.initialized) return;
		this.initialized = false;
		this.disposers.forEach((dispose) => dispose());
		this.disposers = [];
		if (this.globalKeybindTriggeredUnsubscribe) {
			this.globalKeybindTriggeredUnsubscribe();
			this.globalKeybindTriggeredUnsubscribe = null;
		}
		this.stopGlobalKeyHook();
		this.detachLocalHoldListener();
		this.detachLocalKeyboardShortcutListener();
		this.detachLocalMouseShortcutListener();
		this.detachLocalEditableShortcutCaptureListener();
		this.stopGamepadPolling();
		this.holdBindings = [];
		this.activeGlobalShortcutPressIds.clear();
		this.activeLocalShortcutPressIds.clear();
		const electronApi = getElectronAPI();
		if (electronApi) {
			if (this.registeredGlobalHookShortcutIds.size > 0) {
				void electronApi.globalKeyHookUnregisterAll?.().catch(() => {});
			}
		}
		this.registeredGlobalHookShortcutIds.clear();
		this.handlers.clear();
		this.combokeys?.detach();
		this.combokeys = null;
	}

	async startGlobalKeyHook(reason: LinuxInputAccessNagbarReason = 'global-hotkeys'): Promise<boolean> {
		const electronApi = getElectronAPI();
		if (!electronApi?.globalKeyHookStart) return false;
		if (this.globalKeyHookStarted) return true;
		if (!(await this.checkInputMonitoringPermission())) {
			return false;
		}
		const started = await electronApi.globalKeyHookStart();
		if (!started) {
			if (NativePermission.isLinuxWaylandDesktop) {
				void NativePermission.recheckLinuxInputAccess();
				NativePermission.requestLinuxInputAccessNagbar(reason);
			}
			return false;
		}
		this.globalKeyHookStarted = true;
		const keyEventUnsub = electronApi.onGlobalKeyEvent?.((event) => {
			this.handleGlobalKeyEvent(
				event as {
					type: 'keydown' | 'keyup';
					keycode: number;
					keyName: string;
					backend?: 'evdev' | 'native' | null;
					ctrlKey: boolean;
					altKey: boolean;
					shiftKey: boolean;
					metaKey: boolean;
				},
			);
		});
		if (keyEventUnsub) this.globalKeyHookUnsubscribes.push(keyEventUnsub);
		const mouseEventUnsub = electronApi.onGlobalMouseEvent?.((event) => {
			this.handleGlobalMouseEvent(
				event as {
					type: 'mousedown' | 'mouseup';
					button: number;
					ctrlKey: boolean;
					altKey: boolean;
					shiftKey: boolean;
					metaKey: boolean;
				},
			);
		});
		if (mouseEventUnsub) this.globalKeyHookUnsubscribes.push(mouseEventUnsub);
		this.ensureGlobalKeybindTriggeredSubscription();
		return true;
	}

	private ensureGlobalKeybindTriggeredSubscription(): void {
		if (this.globalKeybindTriggeredUnsubscribe) return;
		const electronApi = getElectronAPI();
		this.globalKeybindTriggeredUnsubscribe =
			electronApi?.onGlobalKeybindTriggered?.((event) => {
				if (!this.registeredGlobalHookShortcutIds.has(event.id)) {
					return;
				}
				const keybind = this.resolveGlobalShortcutEventId(event.id);
				if (!keybind) return;
				const isHoldAction = HOLD_ACTIONS.includes(keybind.action as HoldAction);
				if (event.type === 'keyup') {
					if (!this.activeGlobalShortcutPressIds.delete(event.id) && isHoldAction) return;
					if (!isHoldAction) return;
				} else {
					if (this.activeGlobalShortcutPressIds.has(event.id)) return;
					this.activeGlobalShortcutPressIds.add(event.id);
				}
				const handler = this.handlers.get(keybind.action);
				if (!handler) return;
				if (this.suspended) return;
				if (!this.isActionAllowedForCurrentView(keybind.action)) return;
				if (event.type === 'keydown' && shouldSuppressShortcutForFullscreenMedia()) return;
				if (Keybind.isActionMuted(keybind.action)) return;
				handler({type: event.type === 'keydown' ? 'press' : 'release', source: 'global'});
			}) ?? null;
	}

	stopGlobalKeyHook(): void {
		const electronApi = getElectronAPI();
		this.globalKeyHookUnsubscribes.forEach((unsub) => unsub());
		this.globalKeyHookUnsubscribes = [];
		this.releaseGlobalHoldBindings();
		this.releaseActiveGlobalTriggeredHolds();
		this.activeGlobalShortcutPressIds.clear();
		if (electronApi?.globalKeyHookStop && this.globalKeyHookStarted) {
			void electronApi.globalKeyHookStop();
		}
		this.globalKeyHookStarted = false;
	}

	private maybeStopGlobalKeyHook(): void {
		if (this.registeredGlobalHookShortcutIds.size > 0) return;
		if (this.hasGlobalHoldBindings()) return;
		this.stopGlobalKeyHook();
	}

	private releaseActiveGlobalTriggeredHolds(): void {
		for (const id of this.activeGlobalShortcutPressIds) {
			const keybind = this.resolveGlobalShortcutEventId(id);
			if (!keybind || !HOLD_ACTIONS.includes(keybind.action as HoldAction)) continue;
			const handler = this.handlers.get(keybind.action);
			if (handler) {
				handler({type: 'release', source: 'global'});
			}
		}
	}

	private hasGlobalHoldBindings(): boolean {
		return this.holdBindings.some((binding) => binding.routing === 'global');
	}

	private releaseGlobalHoldBindings(): void {
		for (const binding of this.holdBindings) {
			if (binding.routing !== 'global') continue;
			if (binding.pressedKeycodes.size > 0) {
				binding.pressedKeycodes.clear();
				this.fireHoldHandler(binding, 'release', 'global');
			}
			if (binding.globalMouseActive) {
				binding.globalMouseActive = false;
				this.fireHoldHandler(binding, 'release', 'global');
			}
		}
	}

	private releaseHoldBindingsForSuspension(): void {
		this.detachLocalHoldListener();
		this.releaseGlobalHoldBindings();
		this.releaseGamepadHoldBindings();
	}

	private handleGlobalKeyEvent(event: {
		type: 'keydown' | 'keyup';
		keycode: number;
		keyName: string;
		backend?: 'evdev' | 'native' | null;
		ctrlKey: boolean;
		altKey: boolean;
		shiftKey: boolean;
		metaKey: boolean;
	}): void {
		for (const binding of this.holdBindings) {
			if (binding.routing !== 'global') continue;
			this.handleGlobalKeyEventForBinding(binding, event);
		}
	}

	private handleGlobalKeyEventForBinding(
		binding: HoldBindingRuntime,
		event: {
			type: 'keydown' | 'keyup';
			keycode: number;
			keyName: string;
			backend?: 'evdev' | 'native' | null;
			ctrlKey: boolean;
			altKey: boolean;
			shiftKey: boolean;
			metaKey: boolean;
		},
	): void {
		if (binding.isModifierOnly) {
			if (!this.isHoldModifierEvent(binding, event)) return;
			const requiredCount = this.requiredModifierKeyCount(binding);
			if (event.type === 'keydown') {
				if (binding.pressedKeycodes.has(event.keycode)) return;
				binding.pressedKeycodes.add(event.keycode);
				if (binding.pressedKeycodes.size === requiredCount) {
					this.fireHoldHandler(binding, 'press', 'global');
				}
			} else {
				if (!binding.pressedKeycodes.has(event.keycode)) return;
				const wasAtThreshold = binding.pressedKeycodes.size === requiredCount;
				binding.pressedKeycodes.delete(event.keycode);
				if (wasAtThreshold) {
					this.fireHoldHandler(binding, 'release', 'global');
				}
			}
			return;
		}
		if (binding.keycode === null && binding.keyName === null) return;
		if (!this.globalKeyEventMatchesHoldBinding(binding, event)) return;
		if (event.type === 'keyup') {
			if (!binding.pressedKeycodes.delete(event.keycode)) return;
			this.fireHoldHandler(binding, 'release', 'global');
			return;
		}
		if (!this.globalHoldModifiersMatch(binding, event)) return;
		if (binding.pressedKeycodes.has(event.keycode)) return;
		binding.pressedKeycodes.add(event.keycode);
		this.fireHoldHandler(binding, 'press', 'global');
	}

	private globalKeyEventMatchesHoldBinding(
		binding: HoldBindingRuntime,
		event: {
			keycode: number;
			keyName: string;
			backend?: 'evdev' | 'native' | null;
		},
	): boolean {
		const expectedName =
			event.backend === 'evdev' || (event.backend === 'native' && isNativeMacOS())
				? (binding.physicalKeyName ?? binding.keyName)
				: binding.keyName;
		if (expectedName !== null && event.keyName === expectedName) {
			return true;
		}
		return binding.keycode !== null && event.keycode === binding.keycode;
	}

	private isHoldPrimaryModifierDown(event: {ctrlKey: boolean; metaKey: boolean}): boolean {
		return isNativeMacOS() ? event.metaKey : event.ctrlKey;
	}

	private globalHoldModifiersMatch(
		binding: HoldBindingRuntime,
		event: {
			ctrlKey: boolean;
			altKey: boolean;
			shiftKey: boolean;
			metaKey: boolean;
		},
	): boolean {
		const {modifiers, ctrlOrMeta} = binding;
		const anyModifierExpected = ctrlOrMeta || modifiers.ctrl || modifiers.alt || modifiers.shift || modifiers.meta;
		if (!anyModifierExpected) return true;
		if (ctrlOrMeta && !this.isHoldPrimaryModifierDown(event)) return false;
		if (event.ctrlKey !== modifiers.ctrl && !ctrlOrMeta) return false;
		if (event.metaKey !== modifiers.meta && !ctrlOrMeta) return false;
		if (modifiers.ctrl && !event.ctrlKey) return false;
		if (modifiers.meta && !event.metaKey) return false;
		if (event.altKey !== modifiers.alt) return false;
		if (event.shiftKey !== modifiers.shift) return false;
		return true;
	}

	private isHoldModifierEvent(
		binding: HoldBindingRuntime,
		event: {
			keycode: number;
			keyName: string;
		},
	): boolean {
		const {modifiers, ctrlOrMeta} = binding;
		const keyName = event.keyName;
		if (ctrlOrMeta) {
			if (isNativeMacOS()) {
				return keyName === 'MetaLeft' || keyName === 'MetaRight';
			}
			return keyName === 'ControlLeft' || keyName === 'ControlRight';
		}
		if (modifiers.ctrl && (keyName === 'ControlLeft' || keyName === 'ControlRight')) return true;
		if (modifiers.alt && (keyName === 'AltLeft' || keyName === 'AltRight')) return true;
		if (modifiers.shift && (keyName === 'ShiftLeft' || keyName === 'ShiftRight')) return true;
		if (modifiers.meta && (keyName === 'MetaLeft' || keyName === 'MetaRight')) return true;
		return false;
	}

	private handleGlobalMouseEvent(event: {
		type: 'mousedown' | 'mouseup';
		button: number;
		ctrlKey: boolean;
		altKey: boolean;
		shiftKey: boolean;
		metaKey: boolean;
	}): void {
		for (const binding of this.holdBindings) {
			if (binding.routing !== 'global') continue;
			if (binding.mouseButton === null) continue;
			if (event.button !== binding.mouseButton) continue;
			if (event.type === 'mouseup') {
				if (!binding.globalMouseActive) continue;
				binding.globalMouseActive = false;
				this.fireHoldHandler(binding, 'release', 'global');
				continue;
			}
			if (!this.globalHoldModifiersMatch(binding, event)) continue;
			if (binding.globalMouseActive) continue;
			binding.globalMouseActive = true;
			this.fireHoldHandler(binding, 'press', 'global');
		}
	}

	suspend(): void {
		this.manualSuspendCount += 1;
		this.combokeys?.reset();
		this.releaseHoldBindingsForSuspension();
		this.detachLocalEditableShortcutCaptureListener();
		void this.enqueueInputSync(() => {
			this.stopGlobalKeyHook();
		});
	}

	resume(): void {
		this.manualSuspendCount = Math.max(0, this.manualSuspendCount - 1);
		if (!this.suspended) {
			this.refreshLocalShortcuts();
			void this.enqueueInputSync(() => this.applyGlobalShortcuts(this.computeDesiredGlobalHookShortcuts()));
			void this.enqueueInputSync(() => this.applyHoldBindings(this.buildHoldBindings()));
		}
	}

	isSuspended(): boolean {
		return this.suspended;
	}

	private setRouteSuspended(value: boolean): void {
		if (this.routeSuspended === value) return;
		this.routeSuspended = value;
		if (value) {
			this.combokeys?.reset();
			this.releaseHoldBindingsForSuspension();
			this.detachLocalEditableShortcutCaptureListener();
			void this.enqueueInputSync(() => {
				this.stopGlobalKeyHook();
			});
		}
		if (!this.suspended) {
			this.refreshLocalShortcuts();
			void this.enqueueInputSync(() => this.applyGlobalShortcuts(this.computeDesiredGlobalHookShortcuts()));
			void this.enqueueInputSync(() => this.applyHoldBindings(this.buildHoldBindings()));
		}
	}

	private isActionAllowedForCurrentView(action: KeybindCommand): boolean {
		if (this.routeSuspended && !ROUTE_ALLOWED_ACTIONS.has(action)) return false;
		if (this.isCompactCallTextareaActionBlocked(action)) return false;
		return !VoiceCallFullscreen.isActive || isKeybindAllowedDuringVoiceCallFullscreen(action);
	}

	private isCompactCallTextareaActionBlocked(action: KeybindCommand): boolean {
		const channelId = this.currentChannelId;
		if (!channelId) return false;
		const channel = Channels.getChannel(channelId);
		if (!channel) return false;
		if (channel.type === ChannelTypes.GUILD_VOICE) {
			return isKeybindBlockedByCompactVoiceCallView({
				action,
				channelType: channel.type,
				isPrivateChannel: false,
				isGuildVoiceCallExpanded: CompactVoiceCallHeight.getExpandedForKey(
					getGuildVoiceCallExpansionKey(channel.id),
					true,
				),
				isConnectedToPrivateCall: false,
				isPrivateCompactCallExpanded: false,
			});
		}
		if (!channel.isPrivate()) return false;
		if (MediaEngine.channelId !== channel.id || !MediaEngine.connected) return false;
		const call = CallState.getCall(channel.id);
		if (!call) return false;
		return isKeybindBlockedByCompactVoiceCallView({
			action,
			channelType: channel.type,
			isPrivateChannel: true,
			isGuildVoiceCallExpanded: false,
			isConnectedToPrivateCall: true,
			isPrivateCompactCallExpanded: CompactVoiceCallHeight.getExpandedForKey(
				getCompactVoiceCallExpansionKey(channel.id, call.messageId ?? null),
				false,
			),
		});
	}

	register(action: KeybindCommand, handler: KeybindHandler) {
		this.handlers.set(action, handler);
	}

	private registerDefaultHandlers(i18n: I18n) {
		registerMessageHandlers(this, i18n);
		registerDefaultKeybindHandlers(this, i18n);
	}

	private resolveGlobalShortcutEventId(id: string):
		| (KeybindConfig & {
				combo: KeyCombo;
		  })
		| null {
		const currentKeybinds = this.activeKeybinds;
		const actionKeybind = currentKeybinds.find((keybind) => keybind.action === id);
		if (actionKeybind) return actionKeybind;
		const hookKeybind = currentKeybinds.find((keybind) => hookShortcutIdForKeybind(keybind) === id);
		if (hookKeybind) return hookKeybind;
		return null;
	}

	private isHookShortcutRegistered(binding: RuntimeKeybind): boolean {
		return this.isHookShortcutIdRegistered(binding.action, binding.combo);
	}

	private isHookShortcutIdRegistered(action: KeybindCommand, combo: KeyCombo): boolean {
		const id = hookShortcutIdForAction(action, combo);
		return id !== null && this.registeredGlobalHookShortcutIds.has(id);
	}

	private computeDesiredGlobalHookShortcuts(): Map<string, KeyCombo> {
		const desiredCombos = new Map<string, KeyCombo>();
		for (const k of this.activeGlobalKeybinds) {
			const shortcut = hookShortcutIdForKeybind(k);
			if (shortcut && !desiredCombos.has(shortcut)) {
				desiredCombos.set(shortcut, k.combo);
			}
		}
		for (const k of this.activeMouseShortcutKeybinds) {
			if (!k.allowGlobal || (k.combo.global ?? false) !== true) continue;
			const shortcut = hookShortcutIdForKeybind(k);
			if (shortcut && !desiredCombos.has(shortcut)) {
				desiredCombos.set(shortcut, k.combo);
			}
		}
		return desiredCombos;
	}

	private async applyGlobalShortcuts(desiredCombos: ReadonlyMap<string, KeyCombo>): Promise<void> {
		const electronApi = getElectronAPI();
		if (!electronApi) return;
		if (this.suspended || !this.initialized) {
			this.stopGlobalKeyHook();
			return;
		}
		const desired = new Set(desiredCombos.keys());
		const previouslyHookRegistered = new Set(this.registeredGlobalHookShortcutIds);
		for (const shortcut of previouslyHookRegistered) {
			if (desired.has(shortcut)) continue;
			try {
				this.releaseGlobalRegistration(shortcut);
				await electronApi.globalKeyHookUnregister?.(shortcut);
			} catch (error) {
				this.logger.error(`Failed to unregister global hook shortcut ${shortcut}`, error);
			}
			this.registeredGlobalHookShortcutIds.delete(shortcut);
			this.activeGlobalShortcutPressIds.delete(shortcut);
		}
		for (const shortcut of desired) {
			if (this.registeredGlobalHookShortcutIds.has(shortcut)) continue;
			const desiredCombo = desiredCombos.get(shortcut);
			if (!desiredCombo) continue;
			if (await this.tryRegisterGlobalHookShortcut(shortcut, desiredCombo)) {
				this.registeredGlobalHookShortcutIds.add(shortcut);
			}
		}
		if (this.registeredGlobalHookShortcutIds.size > 0 && !this.globalKeyHookStarted) {
			const started = await this.startGlobalKeyHook();
			if (!started) {
				this.registeredGlobalHookShortcutIds.clear();
				this.activeGlobalShortcutPressIds.clear();
				void electronApi.globalKeyHookUnregisterAll?.().catch((error) => {
					this.logger.error('Failed to unregister global hook shortcuts after hook start failure', error);
				});
			}
		}
		this.maybeStopGlobalKeyHook();
	}

	private releaseGlobalRegistration(shortcutId: string): void {
		for (const binding of this.holdBindings) {
			if (binding.routing !== 'global') continue;
			if (hookShortcutIdForAction(binding.action, binding.combo) !== shortcutId) continue;
			if (binding.pressedKeycodes.size > 0) {
				binding.pressedKeycodes.clear();
				this.fireHoldHandler(binding, 'release', 'global');
			}
			if (binding.globalMouseActive) {
				binding.globalMouseActive = false;
				this.fireHoldHandler(binding, 'release', 'global');
			}
		}
	}

	private async tryRegisterGlobalHookShortcut(shortcutId: string, combo: KeyCombo): Promise<boolean> {
		const electronApi = getElectronAPI();
		if (!electronApi?.globalKeyHookRegister) return false;
		const mouseButton = combo.mouseButton;
		const keycode = mouseButton == null ? jsKeyToUiohookKeycode(combo.code ?? combo.key) : null;
		const keyName = mouseButton == null ? keyNameForGlobalHook(combo) : null;
		const physicalKeyName = mouseButton == null ? physicalKeyNameForGlobalHook(combo) : null;
		if (mouseButton == null) {
			if (keycode == null && !keyName) return false;
		}
		const started = await this.startGlobalKeyHook();
		if (!started) return false;
		try {
			await electronApi.globalKeyHookRegister({
				id: shortcutId,
				description: this.resolveGlobalShortcutEventId(shortcutId)?.label,
				...(keycode != null ? {keycode} : {}),
				...(keyName ? {keyName} : {}),
				...(physicalKeyName ? {physicalKeyName} : {}),
				...(mouseButton != null ? {mouseButton} : {}),
				ctrl: Boolean(combo.ctrl) || (!isNativeMacOS() && Boolean(combo.ctrlOrMeta)),
				alt: Boolean(combo.alt),
				shift: Boolean(combo.shift),
				meta: Boolean(combo.meta) || (isNativeMacOS() && Boolean(combo.ctrlOrMeta)),
			});
			return true;
		} catch (error) {
			this.logger.error(`Failed to register global hook shortcut ${shortcutId}`, error);
			return false;
		}
	}

	private refreshLocalShortcuts() {
		if (!this.combokeys || this.suspended) return;
		this.combokeys.reset();
		this.activeLocalShortcutPressIds.clear();
		this.detachLocalKeyboardShortcutListener();
		this.detachLocalMouseShortcutListener();
		this.detachLocalEditableShortcutCaptureListener();
		this.releaseGamepadShortcutStates();
		const groups = new Map<string, Array<RuntimeKeybind>>();
		for (const entry of this.activeKeybinds) {
			if (!this.canBindLocalShortcut(entry)) continue;
			for (const shortcut of comboToCombokeysStrings(entry.combo)) {
				const entries = groups.get(shortcut);
				if (entries) {
					entries.push(entry);
				} else {
					groups.set(shortcut, [entry]);
				}
			}
		}
		for (const [shortcut, entries] of groups) {
			this.bindLocalShortcutGroup(shortcut, this.orderLocalShortcutGroup(entries));
		}
		this.refreshLocalKeyboardShortcutListener();
		this.refreshLocalEditableShortcutCaptureListener();
		this.refreshLocalMouseShortcutListener();
		this.refreshGamepadPolling();
	}

	private canBindLocalShortcut(entry: RuntimeKeybind): boolean {
		const {combo, action} = entry;
		if (HOLD_ACTIONS.includes(action as HoldAction)) return false;
		const handler = this.handlers.get(action);
		if (!handler) return false;
		if (isWebReservedZoomShortcut(action, combo)) return false;
		const shortcuts = comboToCombokeysStrings(combo);
		if (shortcuts.length === 0) return false;
		return true;
	}

	private orderLocalShortcutGroup(entries: ReadonlyArray<RuntimeKeybind>): Array<RuntimeKeybind> {
		return entries
			.map((entry, index) => ({entry, index}))
			.sort((left, right) => {
				const priorityDiff =
					(LOCAL_SHORTCUT_ACTION_PRIORITY[right.entry.action] ?? 0) -
					(LOCAL_SHORTCUT_ACTION_PRIORITY[left.entry.action] ?? 0);
				return priorityDiff || left.index - right.index;
			})
			.map(({entry}) => entry);
	}

	private shouldIgnoreLocalShortcutEvent(entry: RuntimeKeybind, event: KeyboardEvent): boolean {
		const {combo} = entry;
		const hasModifier = !!(combo.ctrl || combo.ctrlOrMeta || combo.alt || combo.meta);
		if (shouldSuppressShortcutForFullscreenMedia(event)) return true;
		const target = event.target ?? null;
		if (shouldSuppressLocalShortcutForModalFocus(entry, target)) return true;
		if (!isEditableElement(target)) return false;
		if (this.shouldAllowLocalShortcutFromEditable(entry, target)) {
			return false;
		}
		if (!hasModifier) return true;
		if (entry.ignoreWhileTyping) return true;
		return false;
	}

	private shouldAllowLocalShortcutFromEditable(entry: RuntimeKeybind, target: HTMLElement): boolean {
		if (entry.section === 'voice_and_video') return true;
		return (
			isChannelTextareaElement(target) &&
			shouldAllowLocalShortcutForChannelTextarea(entry, getEditableElementValue(target))
		);
	}

	private shouldPreserveEditableDefaultForShortcut(entry: RuntimeKeybind, event: KeyboardEvent): boolean {
		if (entry.preventDefaultInEditable) return false;
		const target = event.target ?? null;
		return entry.section === 'voice_and_video' && isEditableElement(target);
	}

	private dispatchLocalShortcut(entry: RuntimeKeybind, type: 'press' | 'release', event: KeyboardEvent): boolean {
		const {combo, action} = entry;
		const requiresKeyboardMode = entry.requiresKeyboardMode ?? false;
		const requiresMessageFocus = entry.requiresMessageFocus ?? false;
		const registeredHookShortcutId = hookShortcutIdForKeybind(entry);
		const handler = this.handlers.get(action);
		if (!handler) return false;
		if (type === 'press' && event.repeat) return false;
		if (this.suspended) return false;
		if (!this.isActionAllowedForCurrentView(action)) return false;
		if (!keyboardEventTriggerMatchesCombo(combo, event)) {
			return false;
		}
		if (this.shouldIgnoreLocalShortcutEvent(entry, event)) return false;
		if (Keybind.isActionMuted(action)) return false;
		const isRegisteredGlobalShortcut = Boolean(
			(combo.global ?? false) &&
				registeredHookShortcutId &&
				this.registeredGlobalHookShortcutIds.has(registeredHookShortcutId),
		);
		if (requiresKeyboardMode && !KeyboardMode.keyboardModeEnabled) {
			return false;
		}
		if (action === 'chat_focus_textarea' && KeyboardMode.keyboardModeEnabled) {
			return false;
		}
		if (!this.isActionAvailableForLocalShortcut(action)) {
			return false;
		}
		let focusedMessage: Message | null = null;
		let focusedChannel = null;
		if (requiresMessageFocus) {
			focusedMessage = MessageFocus.getFocusedMessage();
			if (!focusedMessage) {
				return false;
			}
			focusedChannel = MessageFocus.getFocusedChannel();
		}
		if (isRegisteredGlobalShortcut && registeredHookShortcutId) {
			if (type === 'press') {
				if (this.activeGlobalShortcutPressIds.has(registeredHookShortcutId)) return false;
				this.activeGlobalShortcutPressIds.add(registeredHookShortcutId);
			} else {
				this.activeGlobalShortcutPressIds.delete(registeredHookShortcutId);
			}
		}
		handler({
			type,
			source: 'local',
			context: focusedMessage ? {focusedMessage, focusedChannel} : undefined,
			shiftKey: event.shiftKey,
		});
		if (
			type === 'press' &&
			action === 'system_toggle_shortcuts_overlay' &&
			shouldUseKeyboardShortcutsOverlayFallbackFromEvent(event)
		) {
			this.scheduleKeyboardShortcutsOverlayFallback();
		}
		if (type === 'press' && !this.shouldPreserveEditableDefaultForShortcut(entry, event)) {
			event.preventDefault();
		}
		return true;
	}

	private scheduleKeyboardShortcutsOverlayFallback(): void {
		const applyFallback = () => {
			Keybind.useKeyboardShortcutsOverlayFallback();
		};
		if (typeof queueMicrotask === 'function') {
			queueMicrotask(applyFallback);
			return;
		}
		setTimeout(applyFallback, 0);
	}

	private bindLocalShortcutGroup(shortcut: string, entries: ReadonlyArray<RuntimeKeybind>): void {
		const wrapHandler = (type: 'press' | 'release') => (event?: KeyboardEvent) => {
			if (!event) return;
			if (type === 'release') {
				if (!this.activeLocalShortcutPressIds.delete(shortcut)) return;
				for (const entry of entries) {
					if (this.dispatchLocalShortcut(entry, type, event)) return;
				}
				return;
			}
			if (event.repeat) return;
			if (this.activeLocalShortcutPressIds.has(shortcut)) return;
			for (const entry of entries) {
				if (this.dispatchLocalShortcut(entry, type, event)) {
					this.activeLocalShortcutPressIds.add(shortcut);
					return;
				}
			}
		};
		const combokeys = this.ensureCombokeys();
		if (combokeys) {
			combokeys.bind(shortcut, wrapHandler('press'), 'keydown');
			combokeys.bind(shortcut, wrapHandler('release'), 'keyup');
		}
	}
}

export default new KeybindManager();
