// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {isChromiumBrowser} from '@app/features/ui/utils/NativeUtils';

const DEFAULT_CHUNK_LENGTH = 180;
const URL_PATTERN = /https?:\/\/([^/\s]+)[^\s]*/g;
const LOW_QUALITY_VOICE_NAMES = new Set([
	'albert',
	'bad news',
	'bahh',
	'bells',
	'boing',
	'bubbles',
	'cellos',
	'deranged',
	'fred',
	'good news',
	'grandma',
	'grandpa',
	'hysterical',
	'junior',
	'organ',
	'princess',
	'ralph',
	'trinoids',
	'whisper',
	'zarvox',
]);
const PREFERRED_VOICE_NAMES_BY_LOCALE: Record<string, Array<string>> = {
	en: ['Google US English', 'Samantha', 'Alex', 'Ava', 'Allison', 'Susan', 'Google UK English Female', 'Daniel'],
	'en-US': ['Google US English', 'Samantha', 'Alex', 'Ava', 'Allison', 'Susan'],
	'en-GB': ['Google UK English Female', 'Daniel', 'Serena', 'Kate'],
};
const synthesisSupported =
	typeof window !== 'undefined' && window.speechSynthesis != null && typeof SpeechSynthesisUtterance !== 'undefined';

export function nativeApiPresent(): boolean {
	return synthesisSupported;
}

export function nativeHasVoices(): boolean {
	return synthesisSupported && getVoices().length > 0;
}

function stripUrlsToDomain(text: string): string {
	return text.replace(URL_PATTERN, (_match, domain) => domain);
}

function truncateAtWordBoundary(text: string, maxLength: number): string {
	if (text.length <= maxLength) {
		return text;
	}
	const truncated = text.slice(0, maxLength);
	const lastSpace = truncated.lastIndexOf(' ');
	if (lastSpace > maxLength * 0.5) {
		return truncated.slice(0, lastSpace);
	}
	return truncated;
}

function normaliseText(text: string): string {
	return text.replace(/\s+/g, ' ').trim();
}

function capTextAtWordBoundary(text: string, maxLength?: number): string {
	if (maxLength === undefined || !Number.isFinite(maxLength)) {
		return text;
	}
	if (maxLength <= 0) {
		return '';
	}
	return truncateAtWordBoundary(text, maxLength);
}

function findChunkBoundary(text: string, maxChunkLength: number): number {
	const limit = Math.min(text.length, maxChunkLength);
	const punctuationBoundary = Math.max(
		text.lastIndexOf('. ', limit),
		text.lastIndexOf('! ', limit),
		text.lastIndexOf('? ', limit),
		text.lastIndexOf('; ', limit),
		text.lastIndexOf(': ', limit),
		text.lastIndexOf(', ', limit),
	);
	if (punctuationBoundary > maxChunkLength * 0.5) {
		return punctuationBoundary + 1;
	}
	const wordBoundary = text.lastIndexOf(' ', limit);
	if (wordBoundary > maxChunkLength * 0.5) {
		return wordBoundary;
	}
	return limit;
}

function chunkText(text: string, maxChunkLength: number): Array<string> {
	const chunkLength = Math.max(1, Math.floor(maxChunkLength));
	const chunks: Array<string> = [];
	let remaining = text;
	while (remaining.length > chunkLength) {
		const boundary = findChunkBoundary(remaining, chunkLength);
		const chunk = remaining.slice(0, boundary).trim();
		if (chunk) {
			chunks.push(chunk);
		}
		remaining = remaining.slice(boundary).trim();
	}
	if (remaining) {
		chunks.push(remaining);
	}
	return chunks;
}

function prepareTextForSpeech(text: string, maxLength?: number): string {
	let processed = stripUrlsToDomain(text);
	processed = normaliseText(processed);
	processed = capTextAtWordBoundary(processed, maxLength);
	return processed;
}

export function createUtteranceTexts(
	text: string,
	maxLength?: number,
	maxChunkLength: number = DEFAULT_CHUNK_LENGTH,
): Array<string> {
	const processed = prepareTextForSpeech(text, maxLength);
	if (!processed) {
		return [];
	}
	return chunkText(processed, maxChunkLength);
}

export function createUtterances(text: string, maxLength?: number): Array<SpeechSynthesisUtterance> {
	if (!synthesisSupported) {
		return [];
	}
	return createUtteranceTexts(text, maxLength).map((chunk) => {
		const utterance = new SpeechSynthesisUtterance(chunk);
		utterance.rate = Accessibility.ttsRate;
		return utterance;
	});
}

function normalizeVoiceName(value: string): string {
	return value
		.toLowerCase()
		.replace(/\s*\([^)]*\)\s*/g, ' ')
		.replace(/\s+/g, ' ')
		.trim();
}

function isLowQualityVoice(voice: SpeechSynthesisVoice): boolean {
	const name = normalizeVoiceName(voice.name);
	const voiceUri = normalizeVoiceName(voice.voiceURI);
	return LOW_QUALITY_VOICE_NAMES.has(name) || LOW_QUALITY_VOICE_NAMES.has(voiceUri);
}

function matchesVoiceName(voice: SpeechSynthesisVoice, preferredName: string): boolean {
	const name = normalizeVoiceName(voice.name);
	const voiceUri = normalizeVoiceName(voice.voiceURI);
	const preferred = normalizeVoiceName(preferredName);
	return name === preferred || voiceUri === preferred || name.includes(preferred) || voiceUri.includes(preferred);
}

function normalizeLocale(value: string): string {
	return value.replace(/_/g, '-').toLowerCase();
}

function voiceMatchesLocale(voice: SpeechSynthesisVoice, locale: string): boolean {
	const voiceLocale = normalizeLocale(voice.lang);
	const targetLocale = normalizeLocale(locale);
	return voiceLocale === targetLocale || voiceLocale.startsWith(`${targetLocale}-`);
}

function voiceMatchesLanguage(voice: SpeechSynthesisVoice, language: string): boolean {
	return normalizeLocale(voice.lang).split('-')[0] === normalizeLocale(language);
}

function getPreferredVoiceNames(locale: string): Array<string> {
	const exact = PREFERRED_VOICE_NAMES_BY_LOCALE[locale];
	if (exact) {
		return exact;
	}
	const language = locale.split('-')[0];
	return PREFERRED_VOICE_NAMES_BY_LOCALE[language] ?? [];
}

function selectFromVoices(
	voices: Array<SpeechSynthesisVoice>,
	preferredNames: Array<string>,
): SpeechSynthesisVoice | null {
	const acceptableVoices = voices.filter((voice) => !isLowQualityVoice(voice));
	if (acceptableVoices.length === 0) {
		return null;
	}
	for (const preferredName of preferredNames) {
		const voice = acceptableVoices.find((voice) => matchesVoiceName(voice, preferredName));
		if (voice) {
			return voice;
		}
	}
	return acceptableVoices.find((voice) => voice.default) ?? acceptableVoices[0];
}

export function selectPreferredVoice(voices: Array<SpeechSynthesisVoice>, locale: string): SpeechSynthesisVoice | null {
	const language = locale.split('-')[0];
	const localeVoice = selectFromVoices(
		voices.filter((voice) => voiceMatchesLocale(voice, locale)),
		getPreferredVoiceNames(locale),
	);
	if (localeVoice) {
		return localeVoice;
	}
	const languageVoice = selectFromVoices(
		voices.filter((voice) => voiceMatchesLanguage(voice, language)),
		getPreferredVoiceNames(language),
	);
	if (languageVoice) {
		return languageVoice;
	}
	return selectFromVoices(voices, []);
}

const NATIVE_KEEPALIVE_INTERVAL_MS = 10000;
let keepAliveTimer: ReturnType<typeof setInterval> | null = null;

function stopNativeKeepAlive(): void {
	if (keepAliveTimer !== null) {
		clearInterval(keepAliveTimer);
		keepAliveTimer = null;
	}
}

function startNativeKeepAlive(): void {
	if (!synthesisSupported || !isChromiumBrowser() || keepAliveTimer !== null) {
		return;
	}
	keepAliveTimer = setInterval(() => {
		if (!synthesisSupported || !window.speechSynthesis.speaking) {
			stopNativeKeepAlive();
			return;
		}
		window.speechSynthesis.pause();
		window.speechSynthesis.resume();
	}, NATIVE_KEEPALIVE_INTERVAL_MS);
}

export function resumeNative(): void {
	if (!synthesisSupported) {
		return;
	}
	window.speechSynthesis.resume();
}

export function speak(utterance: SpeechSynthesisUtterance, voice: SpeechSynthesisVoice | null): void {
	if (!synthesisSupported) {
		return;
	}
	utterance.voice = voice;
	window.speechSynthesis.resume();
	window.speechSynthesis.speak(utterance);
	startNativeKeepAlive();
}

export function cancel(): void {
	if (!synthesisSupported) {
		return;
	}
	stopNativeKeepAlive();
	window.speechSynthesis.cancel();
	window.speechSynthesis.resume();
}

export function getVoices(): Array<SpeechSynthesisVoice> {
	if (!synthesisSupported) {
		return [];
	}
	return window.speechSynthesis.getVoices();
}
