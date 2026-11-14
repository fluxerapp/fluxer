// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import Initialization from '@app/features/app/state/Initialization';
import {TRANSIENT_UI_SELECTOR} from '@app/features/boot_state/BootDomFrame';
import {flushBootHydration} from '@app/features/boot_state/BootHydration';
import {
	acquireBootStateWriteToken,
	type BootStateStore,
	captureBootFrame,
	capturesBootFrames,
	capturesDomBootFrames,
	clearBootState,
	onBootStateCleared,
	pairBootFrame,
	pairedBootFrame,
	resolveBootStateAPI,
	writeBootState,
} from '@app/features/boot_state/BootStateBridge';
import {BOOT_FRAME_SHOWN_MARK} from '@app/features/boot_state/BootStateBrowserFormat';
import {
	BOOT_STATE_MAX_MEMBER_LIST_ROWS,
	BOOT_STATE_MAX_MEMBER_LISTS,
	BOOT_STATE_MAX_MESSAGE_CHANNELS,
	BOOT_STATE_MAX_MESSAGES_PER_CHANNEL,
	type BootStateEncodablePayload,
	type BootStateFrameRecord,
	type BootStateMessageWindow,
	type BootStateSerializableMessage,
	type BootStateUnreadView,
	type BootStateViewportChannel,
	encodeBootStatePayloadSteps,
} from '@app/features/boot_state/BootStateCodec';
import BootStateRestore from '@app/features/boot_state/BootStateRestore';
import {
	captureVisibleImageSources,
	channelViewportAtBottom,
	currentUserAreaOverlayHeight,
	type MeasuredChannelViewport,
	measureChannelViewport,
	measureFrameLayout,
} from '@app/features/boot_state/BootViewport';
import Channels from '@app/features/channel/state/Channels';
import {
	captureRetainedCodeHighlights,
	subscribeRetainedCodeHighlights,
} from '@app/features/code_highlighting/utils/ArboriumHighlighting';
import {snapshotGuildMemberKey} from '@app/features/gateway/snapshot/SnapshotEntities';
import SnapshotSync from '@app/features/gateway/snapshot/SnapshotSync';
import type {StateSnapshotCapture} from '@app/features/gateway/snapshot/SnapshotTypes';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberList from '@app/features/member/state/MemberList';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageReferences, {MessageReferenceState} from '@app/features/messaging/state/MessageReferences';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {collectMessageModelGuildMemberUserIds} from '@app/features/messaging/utils/MessageMemberLoadUtils';
import {captureRetainedMarkdownParses} from '@app/features/messaging/utils/markdown/MarkdownParseCache';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import ReadStates from '@app/features/read_state/state/ReadStates';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import Theme from '@app/features/theme/state/Theme';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import Dimension from '@app/features/ui/state/Dimension';
import LayerManager, {LayerType} from '@app/features/ui/state/LayerManager';
import {flushPendingGuildSettings} from '@app/features/user/commands/UserGuildSettingsCommands';
import UserProfile from '@app/features/user/state/UserProfile';
import {MessageStates} from '@fluxer/constants/src/ChannelConstants';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {reaction} from 'mobx';

const logger = new Logger('BootStateWriter');

const FIRST_WRITE_DELAY_MS = 5_000;
const UNRESTORED_FIRST_WRITE_DELAY_MS = 1_000;
const DIRTY_DEBOUNCE_MS = 20_000;
const DIRTY_MAX_WAIT_MS = 120_000;
const ENCODE_SLICE_MS = 8;
const SCREEN_CHANGE_DEBOUNCE_MS = 2_000;
const SCHEDULED_FRAME_MIN_INTERVAL_MS = 60_000;
const SCHEDULED_FRAME_MAX_PIXELS = 6_000_000;
const CAPTURE_SETTLE_TIMEOUT_MS = 100;
const FRAME_CAPTURE_ATTRIBUTE = 'data-boot-frame-capture';
const FRAME_CAPTURE_STYLE = `html[${FRAME_CAPTURE_ATTRIBUTE}] :is(${TRANSIENT_UI_SELECTOR}) {visibility: hidden !important;}`;
const SYSTEM_DARK_QUERY = '(prefers-color-scheme: dark)';

interface CapturedWindow {
	readonly window: BootStateMessageWindow<Message>;
	readonly viewport: BootStateViewportChannel;
}

function capturedUnreadView(channelId: string): BootStateUnreadView | null {
	const state = ReadStates.getIfExists(channelId);
	const unreadCount = ReadStates.getUnreadCount(channelId);
	if (state === undefined || unreadCount === 0 || state.lastMessageId === null) {
		return null;
	}
	return {
		ackMessageId: state.ackMessageId,
		lastMessageId: state.lastMessageId,
		unreadCount,
		estimated: state.estimated,
		oldestUnreadMessageId: state.oldestUnreadMessageId,
	};
}

function layoutTops(channelId: string, measured: ReadonlyMap<string, number>): ReadonlyMap<string, number> {
	const deferred = Messages.deferredOlderOffsets(channelId);
	const mountedTop = deferred === null ? undefined : measured.get(deferred.mountedId);
	if (deferred === null || mountedTop === undefined) return measured;
	const tops = new Map(measured);
	for (const [id, offset] of deferred.offsets) tops.set(id, mountedTop + offset);
	return tops;
}

function capturedViewport(
	channelId: string,
	atBottom: boolean,
	measured: MeasuredChannelViewport | null,
	kept: ReadonlyArray<Message>,
	all: ReadonlyArray<Message>,
): BootStateViewportChannel {
	const dimensions = Dimension.channelDimensionsFor(channelId);
	const tops = measured === null ? null : layoutTops(channelId, measured.messageTopsPx);
	const first = all[0];
	const firstKept = kept[0];
	let scrollTop: number | null = dimensions?.scrollTop ?? null;
	if (scrollTop !== null && first !== undefined && firstKept !== undefined && first.id !== firstKept.id) {
		const firstTop = tops?.get(first.id);
		const keptTop = tops?.get(firstKept.id);
		scrollTop = firstTop === undefined || keptTop === undefined ? null : scrollTop - (keptTop - firstTop);
	}
	const base = firstKept === undefined ? undefined : tops?.get(firstKept.id);
	let messageTopsPx: Record<string, number> | undefined;
	if (tops != null && base !== undefined) {
		messageTopsPx = {};
		for (const message of kept) {
			const top = tops.get(message.id);
			if (top !== undefined) messageTopsPx[message.id] = Math.round(top - base);
		}
	}
	const lastKept = kept[kept.length - 1];
	const below = lastKept === undefined ? undefined : all[all.indexOf(lastKept) + 1];
	const belowTop = below === undefined ? undefined : tops?.get(below.id);
	const tailHeightPx =
		atBottom || measured === null || belowTop === undefined
			? undefined
			: Math.max(0, Math.round(measured.lastBottomPx - belowTop));
	return {
		unread: capturedUnreadView(channelId),
		atBottom,
		unreadBoundary: ReadStates.captureUnreadBoundary(channelId),
		anchorMessageId: measured?.anchorMessageId ?? null,
		anchorOffsetPx: measured?.anchorOffsetPx ?? 0,
		scrollTop: atBottom ? null : scrollTop,
		scrollHeight: atBottom ? null : (dimensions?.scrollHeight ?? null),
		offsetHeight: atBottom ? null : (dimensions?.offsetHeight ?? null),
		...(messageTopsPx === undefined ? {} : {messageTopsPx}),
		...(tailHeightPx === undefined ? {} : {tailHeightPx}),
	};
}

function captureWindow(channelId: string): CapturedWindow | null {
	const messages = Messages.getMessagesForBootCapture(channelId);
	if (messages === undefined || !messages.ready || messages.error || messages.length === 0) {
		return null;
	}
	const sent = messages.toArray().filter((message) => message.state === MessageStates.SENT);
	if (sent.length === 0) {
		return null;
	}
	const atBottom = channelViewportAtBottom(channelId) ?? Dimension.channelPinnedToEnd(channelId);
	const measured = measureChannelViewport(channelId);
	const anchorIndex =
		atBottom || measured === null ? -1 : sent.findIndex((message) => message.id === measured.anchorMessageId);
	const centre = anchorIndex === -1 ? sent.length - 1 : anchorIndex;
	const end = Math.min(
		sent.length,
		Math.max(centre + BOOT_STATE_MAX_MESSAGES_PER_CHANNEL / 2, BOOT_STATE_MAX_MESSAGES_PER_CHANNEL),
	);
	const start = Math.max(0, end - BOOT_STATE_MAX_MESSAGES_PER_CHANNEL);
	const kept = sent.slice(start, end);
	return {
		window: {
			channelId,
			hasMoreBefore: messages.hasMoreBefore || start > 0,
			hasMoreAfter: messages.hasMoreAfter || end < sent.length,
			messages: kept,
		},
		viewport: capturedViewport(channelId, atBottom, measured, kept, sent),
	};
}

function trackedChannelIds(): Array<string> {
	const ids = new Set<string>();
	const selected = SelectedChannel.currentChannelId;
	if (selected !== null) {
		ids.add(selected);
	}
	for (const visit of SelectedChannel.sortedRecentVisits) {
		ids.add(visit.channelId);
	}
	return Array.from(ids);
}

const GUILD_MEMBER_ENTITY = 'guild_member';

export function bootStateMessageJSON(message: Message): WireMessage {
	const json = message.toBootStateJSON();
	const referencedId = message.messageReference?.message_id;
	if (json.referenced_message !== undefined || referencedId == null) {
		return json;
	}
	const resolution = MessageReferences.getMessageReference(
		message.messageReference?.channel_id ?? message.channelId,
		referencedId,
	);
	switch (resolution.state) {
		case MessageReferenceState.DELETED:
			return {...json, referenced_message: null};
		case MessageReferenceState.LOADED:
			return {...json, referenced_message: resolution.message.toBootStateJSON()};
		case MessageReferenceState.NOT_LOADED:
			return json;
	}
}

function serializableWindow(
	window: BootStateMessageWindow<Message>,
): BootStateMessageWindow<BootStateSerializableMessage> {
	return {
		...window,
		messages: window.messages.map((message) => ({toBootStateJSON: () => bootStateMessageJSON(message)})),
	};
}

function withMessageMembers(
	shell: StateSnapshotCapture,
	windows: ReadonlyArray<BootStateMessageWindow<Message>>,
): StateSnapshotCapture {
	const rows: Record<string, string> = {...shell.entries[GUILD_MEMBER_ENTITY]};
	for (const window of windows) {
		const guildId = Channels.getChannel(window.channelId)?.guildId;
		if (guildId == null) {
			continue;
		}
		for (const userId of collectMessageModelGuildMemberUserIds(window.messages, null)) {
			const key = snapshotGuildMemberKey(guildId, userId);
			const member = GuildMembers.getMember(guildId, userId);
			if (member !== null && rows[key] === undefined) {
				rows[key] = JSON.stringify(member.toJSON());
			}
		}
	}
	return {...shell, entries: {...shell.entries, [GUILD_MEMBER_ENTITY]: rows}};
}

function captureDmProfiles() {
	const channel =
		SelectedChannel.currentChannelId === null ? null : Channels.getChannel(SelectedChannel.currentChannelId);
	if (!channel?.isDM()) return [];
	const profile = UserProfile.getProfile(channel.recipientIds[0]);
	if (profile === null) return [];
	const json = profile.toJSON();
	return JSON.stringify(json).length <= 128 * 1024 ? [json] : [];
}

function captureBootState(accountKey: string, frame: BootStateFrameRecord | null): BootStateEncodablePayload | null {
	flushBootHydration();
	const account = SessionManager.getAccount(accountKey);
	const shell = SnapshotSync.captureDemotionSnapshot(accountKey);
	if (account === null || shell === null) {
		return null;
	}
	const windows: Array<BootStateMessageWindow<Message>> = [];
	const viewports: Record<string, BootStateViewportChannel> = {};
	for (const channelId of trackedChannelIds()) {
		if (windows.length === BOOT_STATE_MAX_MESSAGE_CHANNELS) {
			break;
		}
		const captured = captureWindow(channelId);
		if (captured === null) {
			continue;
		}
		windows.push(captured.window);
		viewports[channelId] = captured.viewport;
	}
	return {
		meta: {
			frame,
			accountKey,
			userId: account.userId,
		},
		shell: withMessageMembers(shell, windows),
		mentions: Object.fromEntries(ReadStates.captureMentionCounts()),
		guildReadState: GuildReadState.captureBootState(),
		messages: windows.map(serializableWindow),
		members: MemberSidebar.captureRecentLists(BOOT_STATE_MAX_MEMBER_LIST_ROWS).slice(0, BOOT_STATE_MAX_MEMBER_LISTS),
		viewport: {
			channels: viewports,
			guildListScrollTop: Dimension.currentGuildListDimensions().scrollTop,
			imageSources: captureVisibleImageSources(),
			codeHighlights: captureRetainedCodeHighlights(
				new Set(windows.flatMap((window) => window.messages.map((message) => message.id))),
			),
			dmProfiles: captureDmProfiles(),
			markdownParses: captureRetainedMarkdownParses(),
			rendererBuild: Config.BUILD_VERSION,
			userAreaOverlayHeightPx: currentUserAreaOverlayHeight(),
		},
	};
}

function framePixels(): number {
	const ratio = window.devicePixelRatio;
	return Math.round(window.innerWidth * ratio) * Math.round(window.innerHeight * ratio);
}

function scheduledCaptureBlocker(lastFrameCaptureAt: number, retry = false): string | null {
	if (!retry && Date.now() - lastFrameCaptureAt < SCHEDULED_FRAME_MIN_INTERVAL_MS) {
		return 'a capture ran recently';
	}
	if (framePixels() > SCHEDULED_FRAME_MAX_PIXELS) {
		return 'the window is too large to capture outside a flush';
	}
	return null;
}

function screenSignature(): string {
	const selected = SelectedChannel.currentChannelId;
	const pinned = selected === null || Dimension.channelPinnedToEnd(selected);
	const scrollTop = selected === null ? null : (Dimension.channelDimensionsFor(selected)?.scrollTop ?? null);
	const ratio = window.devicePixelRatio;
	return JSON.stringify([
		Navigation.pathname,
		Math.round(window.innerWidth * ratio),
		Math.round(window.innerHeight * ratio),
		Theme.effectiveTheme,
		MemberList.isMembersOpen,
		StreamerMode.enabled,
		pinned || scrollTop === null ? null : Math.round(scrollTop),
	]);
}

function frameCaptureBlocker(domFrames: boolean): string | null {
	if (StreamerMode.enabled) {
		return 'streamer mode is on';
	}
	if (!domFrames && document.visibilityState !== 'visible') {
		return 'the window is hidden';
	}
	if (
		!domFrames &&
		Array.from(document.querySelectorAll<HTMLImageElement>('#root img[src]')).some((image) => {
			const rect = image.getBoundingClientRect();
			return (
				!image.complete &&
				rect.width > 0 &&
				rect.height > 0 &&
				rect.bottom > 0 &&
				rect.right > 0 &&
				rect.top < innerHeight &&
				rect.left < innerWidth
			);
		})
	)
		return 'visible images are still loading';
	if (SelectedChannel.currentChannelId === null) {
		return 'no channel is open';
	}
	if (LayerManager.hasType(LayerType.MODAL)) {
		return 'a modal is open';
	}
	if (LayerManager.hasType(LayerType.POPOUT)) {
		return 'a popout is open';
	}
	if (LayerManager.hasType(LayerType.CONTEXT_MENU)) {
		return 'a context menu is open';
	}
	return null;
}

function frameSkipReason(blocker: string | null, scheduledBlocker: string | null, transientUiShown: boolean): string {
	return blocker ?? scheduledBlocker ?? (transientUiShown ? 'transient UI is shown' : 'the capture failed');
}

function systemDarkForFrame(): boolean | null {
	if (ThemeProfile.themePreference !== ThemeTypes.SYSTEM || typeof window.matchMedia !== 'function') {
		return null;
	}
	return window.matchMedia(SYSTEM_DARK_QUERY).matches;
}

function nextPaint(): Promise<void> {
	return new Promise((resolve) => {
		const timer = setTimeout(resolve, CAPTURE_SETTLE_TIMEOUT_MS);
		requestAnimationFrame(() =>
			requestAnimationFrame(() => {
				clearTimeout(timer);
				resolve();
			}),
		);
	});
}

async function withTransientUiHidden<T>(capture: () => Promise<T>): Promise<T> {
	const root = document.documentElement;
	const style = document.createElement('style');
	style.textContent = FRAME_CAPTURE_STYLE;
	document.head.appendChild(style);
	root.setAttribute(FRAME_CAPTURE_ATTRIBUTE, '');
	try {
		await nextPaint();
		return await capture();
	} finally {
		root.removeAttribute(FRAME_CAPTURE_ATTRIBUTE);
		style.remove();
	}
}

function nextSlice(urgent: boolean): Promise<void> {
	return new Promise((resolve) => {
		if (urgent || typeof window.requestIdleCallback !== 'function') {
			setTimeout(resolve, 0);
			return;
		}
		window.requestIdleCallback(() => resolve(), {timeout: 1_000});
	});
}

async function runSliced<T>(steps: Generator<void, T, void>, urgent: boolean): Promise<T> {
	for (;;) {
		await nextSlice(urgent);
		const sliceStart = performance.now();
		for (;;) {
			const step = steps.next();
			if (step.done) {
				return step.value;
			}
			if (performance.now() - sliceStart >= ENCODE_SLICE_MS) {
				break;
			}
		}
	}
}

function restoredThisVisit(domFrames: boolean): boolean {
	return (
		BootStateRestore.phase === 'live' && (!domFrames || performance.getEntriesByName(BOOT_FRAME_SHOWN_MARK).length > 0)
	);
}

class BootStateWriter {
	private armedAccountKey: string | null = null;
	private generation = 0;
	private firstWritePending = false;
	private dirtySince: number | null = null;
	private lastFrameCaptureAt = 0;
	private heldUntil = 0;
	private lastHeldReason: string | null = null;
	private screenVersion = 0;
	private frameScreenVersion = 0;
	private timer: ReturnType<typeof setTimeout> | null = null;
	private inFlight: Promise<void> | null = null;
	private rerunRequested = false;

	private readonly domFrames: boolean;

	constructor(private readonly api: BootStateStore) {
		this.domFrames = capturesDomBootFrames(api);
	}

	install(): () => void {
		const disposeArm = reaction(
			() => {
				const accountKey = Initialization.isReady && !StreamerMode.enabled ? SessionManager.currentAccountKey : null;
				return accountKey !== null && SessionManager.getAccount(accountKey)?.isValid === true ? accountKey : null;
			},
			(accountKey) => this.arm(accountKey),
			{fireImmediately: true},
		);
		const disposeDirty = reaction(
			() => {
				const selected = SelectedChannel.currentChannelId;
				return [
					Messages.version,
					LayerManager.hasType(LayerType.MODAL),
					LayerManager.hasType(LayerType.POPOUT),
					LayerManager.hasType(LayerType.CONTEXT_MENU),
					MemberSidebar.lists,
					MemberList.isMembersOpen,
					UserProfile.profiles,
					StreamerMode.enabled,
					Theme.effectiveTheme,
					Navigation.pathname,
					selected,
					selected === null ? null : Dimension.channelDimensionsFor(selected),
					Dimension.currentGuildListDimensions(),
				];
			},
			() => this.markDirty(),
		);
		const disposeStreamerMode = reaction(
			() => StreamerMode.enabled,
			(enabled) => {
				const accountKey = SessionManager.currentAccountKey;
				if (enabled && accountKey !== null) void clearBootState(accountKey);
			},
		);
		const disposeHighlights = subscribeRetainedCodeHighlights(() => this.markDirty());
		const disposeSnapshot = SnapshotSync.subscribe(() => this.markDirty());
		const disposeFlush =
			this.api.onFlushRequested?.(async (options) => {
				await Promise.all([
					this.flush({frame: options.frame, force: true}),
					flushPendingGuildSettings(),
					ReadStates.flushPendingAcks(),
				]);
			}) ?? (() => {});
		const disposeCleared = onBootStateCleared((accountKey) => {
			if (accountKey === this.armedAccountKey) {
				this.arm(null);
			}
		});
		const onVisibilityChange = () => {
			if (document.visibilityState === 'hidden') {
				void this.flush({frame: true, force: false});
				return;
			}
			this.scheduleDirty();
		};
		const onResize = () => this.markDirty();
		const onImageSettled = (event: Event) => {
			if (event.target instanceof HTMLImageElement && event.target.closest('#root') !== null) this.markDirty();
		};
		document.addEventListener('load', onImageSettled, true);
		document.addEventListener('error', onImageSettled, true);
		document.addEventListener('visibilitychange', onVisibilityChange);
		window.addEventListener('resize', onResize);
		return () => {
			this.arm(null);
			disposeArm();
			disposeDirty();
			disposeStreamerMode();
			disposeSnapshot();
			disposeHighlights();
			disposeFlush();
			disposeCleared();
			document.removeEventListener('visibilitychange', onVisibilityChange);
			document.removeEventListener('load', onImageSettled, true);
			document.removeEventListener('error', onImageSettled, true);
			window.removeEventListener('resize', onResize);
		};
	}

	private arm(accountKey: string | null): void {
		if (accountKey === this.armedAccountKey) {
			return;
		}
		this.generation += 1;
		this.cancelTimer();
		this.armedAccountKey = accountKey;
		this.frameScreenVersion = this.screenVersion - 1;
		this.firstWritePending = accountKey !== null;
		this.dirtySince = accountKey === null ? null : Date.now();
		if (accountKey !== null) {
			this.schedule(restoredThisVisit(this.domFrames) ? FIRST_WRITE_DELAY_MS : UNRESTORED_FIRST_WRITE_DELAY_MS);
		}
	}

	private markDirty(): void {
		if (this.armedAccountKey === null) {
			return;
		}
		this.screenVersion += 1;
		if (this.dirtySince === null) {
			this.dirtySince = Date.now();
		}
		if (this.firstWritePending) {
			this.schedule(this.dirtySince + FIRST_WRITE_DELAY_MS - Date.now());
		} else this.scheduleDirty();
	}

	private scheduleDirty(): void {
		const accountKey = this.armedAccountKey;
		if (
			accountKey === null ||
			this.dirtySince === null ||
			this.firstWritePending ||
			document.visibilityState !== 'visible'
		) {
			return;
		}
		const now = Date.now();
		const dirtyDelay = Math.max(
			Math.min(DIRTY_DEBOUNCE_MS, this.dirtySince + DIRTY_MAX_WAIT_MS - now),
			this.heldUntil - now,
		);
		const screenChanged =
			capturesBootFrames(this.api) &&
			(this.domFrames || framePixels() <= SCHEDULED_FRAME_MAX_PIXELS) &&
			pairedBootFrame(accountKey)?.signature !== screenSignature();
		const screenDelay = this.domFrames
			? SCREEN_CHANGE_DEBOUNCE_MS
			: Math.max(SCREEN_CHANGE_DEBOUNCE_MS, this.lastFrameCaptureAt + SCHEDULED_FRAME_MIN_INTERVAL_MS - now);
		this.schedule(screenChanged ? Math.min(dirtyDelay, screenDelay) : dirtyDelay);
	}

	private schedule(delayMs: number): void {
		this.cancelTimer();
		this.timer = setTimeout(
			() => {
				this.timer = null;
				void this.write(false);
			},
			Math.max(0, delayMs),
		);
	}

	private cancelTimer(): void {
		if (this.timer !== null) {
			clearTimeout(this.timer);
			this.timer = null;
		}
	}

	private async flush(options: {frame: boolean; force: boolean}): Promise<void> {
		this.cancelTimer();
		if (this.inFlight !== null) {
			await this.inFlight;
		}
		const accountKey = this.armedAccountKey;
		if (accountKey === null) {
			return;
		}
		const frameStale =
			capturesBootFrames(this.api) &&
			(this.frameScreenVersion !== this.screenVersion || pairedBootFrame(accountKey)?.signature !== screenSignature());
		if (options.force || this.dirtySince !== null || frameStale) {
			await this.write(true, options.frame);
		}
	}

	private async pairFrame(
		accountKey: string,
		writeToken: string,
		urgent: boolean,
		retry = false,
		allowCapture = true,
	): Promise<BootStateFrameRecord | null | 'held' | 'unstable'> {
		const api = this.api;
		if (!capturesBootFrames(api)) {
			return null;
		}
		const signature = screenSignature();
		const paired = pairedBootFrame(accountKey);
		const blocker = frameCaptureBlocker(this.domFrames);
		const transientUiShown = !this.domFrames && document.querySelector(TRANSIENT_UI_SELECTOR) !== null;
		const scheduledBlocker = urgent || this.domFrames ? null : scheduledCaptureBlocker(this.lastFrameCaptureAt, retry);
		if (allowCapture && blocker === null && scheduledBlocker === null && (urgent || !transientUiShown)) {
			const captureAppCheck = StreamerMode.autoEnable;
			const capturedVersion = this.screenVersion;
			let layout: string | null = null;
			const capture = () => {
				layout = measureFrameLayout();
				return captureBootFrame(api, accountKey, writeToken, {captureAppCheck, systemDark: systemDarkForFrame()});
			};
			const frameId = await (transientUiShown ? withTransientUiHidden(capture) : capture());
			this.lastFrameCaptureAt = Date.now();
			if (StreamerMode.enabled) return null;
			if (capturedVersion !== this.screenVersion) return 'unstable';
			if (frameId !== null) {
				this.frameScreenVersion = capturedVersion;
				const captured = {id: frameId, signature, layout};
				pairBootFrame(accountKey, captured);
				return captured;
			}
		}
		if (
			paired !== null &&
			paired.signature === signature &&
			(!allowCapture || this.frameScreenVersion === this.screenVersion)
		) {
			return paired;
		}
		if (!allowCapture) {
			return null;
		}
		if (paired !== null) {
			const reason = frameSkipReason(blocker, scheduledBlocker, transientUiShown);
			if (reason !== this.lastHeldReason) {
				this.lastHeldReason = reason;
				logger.debug('Keeping the previous boot state until a matching frame can be captured', {reason});
			}
			return 'held';
		}
		return null;
	}

	private write(urgent: boolean, captureFrame = true): Promise<void> {
		if (this.inFlight !== null) {
			this.rerunRequested = true;
			return this.inFlight;
		}
		const run = this.runWrite(urgent, captureFrame).finally(() => {
			if (this.inFlight === run) {
				this.inFlight = null;
			}
			if (this.rerunRequested) {
				this.rerunRequested = false;
				this.scheduleDirty();
			}
		});
		this.inFlight = run;
		return run;
	}

	private async runWrite(urgent: boolean, captureFrame: boolean): Promise<void> {
		const accountKey = this.armedAccountKey;
		const generation = this.generation;
		if (accountKey === null || StreamerMode.enabled) {
			return;
		}
		this.cancelTimer();
		const writeToken = await acquireBootStateWriteToken(this.api, accountKey);
		if (writeToken === null || generation !== this.generation) {
			return;
		}
		let frame = await this.pairFrame(accountKey, writeToken, urgent, false, captureFrame);
		if (generation !== this.generation || StreamerMode.enabled) return;
		if (frame === 'unstable') {
			frame = await this.pairFrame(accountKey, writeToken, urgent, true, captureFrame);
			if (frame === 'unstable') frame = null;
		}
		if (frame === 'held') {
			this.heldUntil = Date.now() + DIRTY_MAX_WAIT_MS;
			return;
		}
		this.heldUntil = 0;
		this.lastHeldReason = null;
		if (generation !== this.generation || StreamerMode.enabled) {
			return;
		}
		const payload = captureBootState(accountKey, frame);
		if (payload === null) {
			return;
		}
		this.dirtySince = null;
		this.firstWritePending = false;
		try {
			const sessionToken = SessionManager.getAccount(accountKey)?.token;
			const bytes = await runSliced(
				encodeBootStatePayloadSteps(payload, {forbiddenMetaValues: sessionToken === undefined ? [] : [sessionToken]}),
				urgent,
			);
			if (generation !== this.generation) {
				return;
			}
			if (bytes === null) {
				await clearBootState(accountKey);
				return;
			}
			const result = await writeBootState(this.api, accountKey, writeToken, bytes, frame?.id ?? null);
			if (result === 'revoked' && generation === this.generation) {
				this.arm(null);
			}
		} catch (error) {
			logger.warn('Failed to write the boot state', {accountKey, error});
		}
	}
}

export function installBootStateWriter(): () => void {
	const api = resolveBootStateAPI();
	if (api === null || !SnapshotSync.enabled) {
		return () => {};
	}
	return new BootStateWriter(api).install();
}
