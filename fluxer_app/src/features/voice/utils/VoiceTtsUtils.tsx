// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import MessageReferences, {MessageReferenceState} from '@app/features/messaging/state/MessageReferences';
import {SystemMessageUtils} from '@app/features/messaging/utils/SystemMessageUtils';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Relationships from '@app/features/relationship/state/Relationships';
import Notification, {TTSNotificationMode} from '@app/features/ui/state/Notification';
import {getNativePlatformSync} from '@app/features/ui/utils/NativeUtils';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as TtsSpeechUtils from '@app/features/voice/utils/TtsSpeechUtils';
import {formatMessageForTts} from '@app/features/voice/utils/TtsTextFormatter';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {type I18n, i18n as linguiI18n} from '@lingui/core';
import {msg, plural} from '@lingui/core/macro';
import {reaction} from 'mobx';

const SENT_A_STICKER_DESCRIPTOR = msg({
	message: 'sent a sticker',
	comment:
		'TTS substitution phrase spoken after the author name when a message contains only a sticker. Lowercase, mid-sentence.',
});
const SENT_AN_ATTACHMENT_DESCRIPTOR = msg({
	message: 'sent an attachment',
	comment:
		'TTS substitution phrase spoken after the author name when a message contains only attachments. Lowercase, mid-sentence.',
});
const SENT_AN_EMBED_DESCRIPTOR = msg({
	message: 'sent an embed',
	comment:
		'TTS substitution phrase spoken after the author name when a message contains only an embed. Lowercase, mid-sentence.',
});
const MESSAGE_DESCRIPTOR = msg({
	message: '{authorName} {description}',
	comment:
		'TTS sentence template combining {authorName} and the speakable {description}. Used for non-text-only messages.',
});
const MAX_RECENT_MESSAGES = 10;

let recentMessageIds: Array<string> = [];
let currentMessage: {channelId: string; messageId: string} | null = null;
let selectedVoice: SpeechSynthesisVoice | null = null;
let availableVoices: Array<SpeechSynthesisVoice> | null = null;
let speaking = false;
let initialized = false;
let disposers: Array<() => void> = [];
let i18n: I18n | null = null;
let speechSessionId = 0;
let fallbackModule: typeof import('@app/features/voice/utils/EspeakTtsFallback') | null = null;

function setI18n(instance: I18n): void {
	i18n = instance;
}

function getI18n(): I18n {
	return i18n ?? linguiI18n;
}

function ensureInitialized(): void {
	if (!initialized) {
		init();
	}
}

async function showUnsupportedModal(): Promise<void> {
	const [ContextMenuCommands, ModalCommands, {TtsUnsupportedModal}] = await Promise.all([
		import('@app/features/ui/commands/ContextMenuCommands'),
		import('@app/features/ui/commands/ModalCommands'),
		import('@app/features/voice/components/alerts/TtsUnsupportedModal'),
	]);
	ContextMenuCommands.close();
	ModalCommands.push(
		ModalCommands.modal(() => (
			<TtsUnsupportedModal data-flx="voice.tts-utils.show-unsupported-modal.tts-unsupported-modal" />
		)),
	);
}

function findVoiceForLocale(locale: string): SpeechSynthesisVoice | null {
	if (availableVoices === null || availableVoices.length === 0) {
		availableVoices = TtsSpeechUtils.getVoices();
	}
	const voices = availableVoices;
	if (voices.length === 0) {
		return null;
	}
	return TtsSpeechUtils.selectPreferredVoice(voices, locale);
}

function handleVoicesChanged(): void {
	availableVoices = null;
	refreshVoices();
}

function refreshVoices(): void {
	const locale = UserSettings.locale;
	selectedVoice = findVoiceForLocale(locale);
}

function beginSpeechSession(): number {
	speechSessionId += 1;
	return speechSessionId;
}

function isActiveSpeechSession(sessionId: number): boolean {
	return sessionId === speechSessionId;
}

function stopSpeaking(): void {
	speechSessionId += 1;
	TtsSpeechUtils.cancel();
	fallbackModule?.cancel();
	currentMessage = null;
	speaking = false;
}

function nativeSpeechEngineUnavailable(): boolean {
	return getNativePlatformSync() === 'linux' && !TtsSpeechUtils.nativeHasVoices();
}

function canUseNativeSpeech(): boolean {
	return TtsSpeechUtils.nativeApiPresent() && !nativeSpeechEngineUnavailable();
}

function fallbackAvailable(): boolean {
	return (
		typeof window !== 'undefined' &&
		(typeof AudioContext !== 'undefined' ||
			typeof (window as {webkitAudioContext?: unknown}).webkitAudioContext !== 'undefined')
	);
}

async function loadFallbackModule(): Promise<typeof import('@app/features/voice/utils/EspeakTtsFallback')> {
	if (!fallbackModule) {
		fallbackModule = await import('@app/features/voice/utils/EspeakTtsFallback');
	}
	return fallbackModule;
}

function addRecentMessageId(messageId: string): void {
	recentMessageIds = [messageId, ...recentMessageIds.filter((id) => id !== messageId)].slice(0, MAX_RECENT_MESSAGES);
}

function hasRecentlySpoken(messageId: string): boolean {
	return recentMessageIds.includes(messageId);
}

interface SpeakTextOptions {
	text: string;
	interrupt?: boolean;
	maxLength?: number;
	rate?: number;
	onStart?: () => void;
	onEnd?: () => void;
	onError?: (error: string) => void;
}

function completeSpeechSession(sessionId: number, onEnd?: () => void): void {
	if (!isActiveSpeechSession(sessionId)) {
		return;
	}
	speaking = false;
	currentMessage = null;
	onEnd?.();
}

interface SpeakCallbacks {
	onStart?: () => void;
	onEnd?: () => void;
	onError?: (error: string) => void;
}

function speakText(options: SpeakTextOptions): void {
	ensureInitialized();
	const {text, interrupt = true, maxLength, rate, onStart, onEnd, onError} = options;
	if (interrupt) {
		stopSpeaking();
	}
	const sessionId = beginSpeechSession();
	const callbacks: SpeakCallbacks = {onStart, onEnd, onError};
	if (canUseNativeSpeech()) {
		speakWithNative(text, maxLength, rate, sessionId, callbacks);
		return;
	}
	if (fallbackAvailable()) {
		void speakWithFallback(text, maxLength, rate, sessionId, callbacks);
		return;
	}
	currentMessage = null;
	speaking = false;
	onError?.('unsupported');
}

function speakWithNative(
	text: string,
	maxLength: number | undefined,
	rate: number | undefined,
	sessionId: number,
	callbacks: SpeakCallbacks,
): void {
	if (selectedVoice === null) {
		refreshVoices();
	}
	const utterances = TtsSpeechUtils.createUtterances(text, maxLength);
	if (utterances.length === 0) {
		currentMessage = null;
		speaking = false;
		return;
	}
	let nextUtteranceIndex = 0;
	let hasStarted = false;
	const speakNextUtterance = () => {
		if (!isActiveSpeechSession(sessionId)) {
			return;
		}
		const utterance = utterances[nextUtteranceIndex];
		nextUtteranceIndex += 1;
		if (rate !== undefined) {
			utterance.rate = rate;
		}
		utterance.onstart = () => {
			if (!isActiveSpeechSession(sessionId)) {
				return;
			}
			speaking = true;
			if (!hasStarted) {
				hasStarted = true;
				callbacks.onStart?.();
			}
		};
		utterance.onend = () => {
			if (!isActiveSpeechSession(sessionId)) {
				return;
			}
			if (nextUtteranceIndex < utterances.length) {
				speakNextUtterance();
				return;
			}
			completeSpeechSession(sessionId, callbacks.onEnd);
		};
		utterance.onerror = (event) => {
			if (!isActiveSpeechSession(sessionId)) {
				return;
			}
			if (event.error === 'canceled' || event.error === 'interrupted') {
				return;
			}
			speaking = false;
			currentMessage = null;
			callbacks.onError?.(event.error);
		};
		TtsSpeechUtils.speak(utterance, selectedVoice);
	};
	speakNextUtterance();
}

async function speakWithFallback(
	text: string,
	maxLength: number | undefined,
	rate: number | undefined,
	sessionId: number,
	callbacks: SpeakCallbacks,
): Promise<void> {
	const chunks = TtsSpeechUtils.createUtteranceTexts(text, maxLength);
	if (chunks.length === 0) {
		currentMessage = null;
		speaking = false;
		return;
	}
	let fallback: typeof import('@app/features/voice/utils/EspeakTtsFallback');
	try {
		fallback = await loadFallbackModule();
	} catch {
		if (!isActiveSpeechSession(sessionId)) {
			return;
		}
		speaking = false;
		currentMessage = null;
		callbacks.onError?.('unsupported');
		return;
	}
	if (!isActiveSpeechSession(sessionId)) {
		return;
	}
	const speed = fallback.rateToSpeed(rate ?? Accessibility.ttsRate);
	const voiceKey = fallback.resolveVoiceKey(UserSettings.locale);
	let nextChunkIndex = 0;
	let hasStarted = false;
	const speakNextChunk = async () => {
		if (!isActiveSpeechSession(sessionId)) {
			return;
		}
		if (nextChunkIndex >= chunks.length) {
			completeSpeechSession(sessionId, callbacks.onEnd);
			return;
		}
		const chunk = chunks[nextChunkIndex];
		nextChunkIndex += 1;
		let wav: ArrayBuffer | null;
		try {
			wav = await fallback.synthesize(chunk, {speed, voiceKey});
		} catch {
			if (!isActiveSpeechSession(sessionId)) {
				return;
			}
			speaking = false;
			currentMessage = null;
			callbacks.onError?.('synthesis-failed');
			return;
		}
		if (!isActiveSpeechSession(sessionId)) {
			return;
		}
		if (!wav) {
			void speakNextChunk();
			return;
		}
		speaking = true;
		if (!hasStarted) {
			hasStarted = true;
			callbacks.onStart?.();
		}
		const handle = await fallback.play(wav, () => {
			if (!isActiveSpeechSession(sessionId)) {
				return;
			}
			void speakNextChunk();
		});
		if (!isActiveSpeechSession(sessionId)) {
			handle?.stop();
		}
	};
	void speakNextChunk();
}

function speakMessage(content: string): void {
	ensureInitialized();
	if (!isSupported()) {
		void showUnsupportedModal();
		return;
	}
	if (speaking) {
		stopSpeaking();
		return;
	}
	speakText({text: content});
}

function isUserMessageType(type: number): boolean {
	return type === MessageTypes.DEFAULT || type === MessageTypes.REPLY || type === MessageTypes.CLIENT_SYSTEM;
}

function describeNonTextContent(message: Message, localI18n: I18n): string | null {
	if (message.stickers && message.stickers.length > 0) {
		return localI18n._(SENT_A_STICKER_DESCRIPTOR);
	}
	if (message.attachments && message.attachments.length > 0) {
		if (message.attachments.length === 1) {
			return localI18n._(SENT_AN_ATTACHMENT_DESCRIPTOR);
		}
		return localI18n._(
			plural(
				{count: message.attachments.length},
				{
					one: 'sent # attachment',
					other: 'sent # attachments',
				},
			),
		);
	}
	if (message.embeds && message.embeds.length > 0) {
		return localI18n._(SENT_AN_EMBED_DESCRIPTOR);
	}
	return null;
}

function shouldSpeakMessage(message: Message): boolean {
	const mode = Notification.ttsNotificationMode;
	const isExplicitTts = message.tts === true;
	const isSelf = message.author.id === Authentication.currentUserId;
	const isSystemMessage = !isUserMessageType(message.type);
	if (isSelf && !isSystemMessage) {
		return false;
	}
	if (mode === TTSNotificationMode.NEVER) {
		return isExplicitTts && Accessibility.enableTTSCommand;
	}
	if (mode === TTSNotificationMode.FOR_CURRENT_CHANNEL) {
		if (message.channel_id !== SelectedChannel.currentChannelId) {
			return false;
		}
	}
	return true;
}

function handleIncomingTtsMessage(message: Message): void {
	if (!shouldSpeakMessage(message)) {
		return;
	}
	ensureInitialized();
	if (!isSupported()) {
		return;
	}
	if (hasRecentlySpoken(message.id)) {
		return;
	}
	if (Relationships.isBlocked(message.author.id)) {
		return;
	}
	const channel = Channels.getChannel(message.channel_id);
	if (!channel) {
		return;
	}
	if (
		UserGuildSettings.isGuildOrChannelMuted(channel.guildId ?? null, channel.id) ||
		UserGuildSettings.isParentCategoryMuted(channel.guildId ?? null, channel.id)
	) {
		return;
	}
	const localI18n = getI18n();
	if (!isUserMessageType(message.type)) {
		const systemText = SystemMessageUtils.stringify(message, localI18n);
		if (!systemText) {
			return;
		}
		addRecentMessageId(message.id);
		currentMessage = {channelId: message.channel_id, messageId: message.id};
		speakText({text: systemText});
		return;
	}
	const author = Users.getUser(message.author.id);
	if (!author) {
		return;
	}
	const authorName = NicknameUtils.getNickname(author, channel.guildId ?? null);
	if (!message.content.trim()) {
		const description = describeNonTextContent(message, localI18n);
		if (!description) {
			return;
		}
		addRecentMessageId(message.id);
		currentMessage = {channelId: message.channel_id, messageId: message.id};
		speakText({text: localI18n._(MESSAGE_DESCRIPTOR, {authorName, description})});
		return;
	}
	let replyAuthorName: string | null = null;
	if (message.message_reference?.message_id) {
		const refChannelId = message.message_reference.channel_id ?? message.channel_id;
		const refMessageId = message.message_reference.message_id;
		const ref = MessageReferences.getMessageReference(refChannelId, refMessageId);
		if (ref.state === MessageReferenceState.LOADED) {
			const replyAuthor = Users.getUser(ref.message.author.id);
			if (replyAuthor) {
				replyAuthorName = NicknameUtils.getNickname(replyAuthor, channel.guildId ?? null);
			}
		}
	}
	const formattedText = formatMessageForTts(
		message.content,
		authorName,
		channel.guildId ?? null,
		localI18n,
		replyAuthorName,
	);
	addRecentMessageId(message.id);
	currentMessage = {channelId: message.channel_id, messageId: message.id};
	speakText({text: formattedText});
}

function handleMessageDelete(channelId: string, messageId: string): void {
	if (currentMessage?.channelId === channelId && currentMessage?.messageId === messageId) {
		stopSpeaking();
	}
}

function handleChannelSelect(channelId: string | null): void {
	if (currentMessage && currentMessage.channelId !== channelId) {
		stopSpeaking();
	}
}

function init(): void {
	if (initialized) {
		return;
	}
	initialized = true;
	if (!isSupported()) {
		return;
	}
	if (TtsSpeechUtils.nativeApiPresent()) {
		window.speechSynthesis.addEventListener('voiceschanged', handleVoicesChanged);
		TtsSpeechUtils.resumeNative();
	}
	disposers.push(
		reaction(
			() => SelectedChannel.currentChannelId,
			(channelId) => handleChannelSelect(channelId),
		),
	);
	disposers.push(
		reaction(
			() => UserSettings.locale,
			() => refreshVoices(),
		),
	);
}

function dispose(): void {
	if (!initialized) {
		return;
	}
	stopSpeaking();
	if (TtsSpeechUtils.nativeApiPresent()) {
		window.speechSynthesis.removeEventListener('voiceschanged', handleVoicesChanged);
	}
	for (const disposer of disposers) {
		disposer();
	}
	disposers = [];
	recentMessageIds = [];
	selectedVoice = null;
	availableVoices = null;
	initialized = false;
}

function isSupported(): boolean {
	return canUseNativeSpeech() || fallbackAvailable();
}

function isSpeaking(): boolean {
	return speaking;
}

function hasVoices(): boolean {
	ensureInitialized();
	return isSupported();
}

function isUsingFallback(): boolean {
	return !canUseNativeSpeech() && fallbackAvailable();
}

interface SpeakOptions {
	rate?: number;
	onEnd?: () => void;
	onError?: (error: string) => void;
}

function speak(text: string, options?: SpeakOptions): void {
	ensureInitialized();
	speakText({
		text,
		maxLength: 2000,
		rate: options?.rate,
		onEnd: options?.onEnd,
		onError: options?.onError,
	});
}

function stop(): void {
	stopSpeaking();
}

export default {
	init,
	dispose,
	setI18n,
	isSupported,
	isSpeaking,
	hasVoices,
	isUsingFallback,
	speak,
	stop,
	speakMessage,
	stopSpeaking,
	handleIncomingTtsMessage,
	handleMessageDelete,
	handleChannelSelect,
};
