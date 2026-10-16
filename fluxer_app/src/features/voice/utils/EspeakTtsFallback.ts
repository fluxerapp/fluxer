// SPDX-License-Identifier: AGPL-3.0-or-later

import * as SoundUtils from '@app/features/notification/utils/SoundUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import meSpeak from 'mespeak';
import meSpeakConfig from 'mespeak/src/mespeak_config.json';
import enUsVoice from 'mespeak/voices/en/en-us.json';

const logger = new Logger('EspeakTtsFallback');
const ESPEAK_BASE_WPM = 175;
const ESPEAK_MIN_WPM = 80;
const ESPEAK_MAX_WPM = 450;
const DEFAULT_VOICE_KEY = 'en';

type VoiceLoader = () => Promise<{default: unknown}>;

const VOICE_LOADERS: Record<string, VoiceLoader> = {
	en: () => Promise.resolve({default: enUsVoice}),
	'en-us': () => Promise.resolve({default: enUsVoice}),
	'en-gb': () => import('mespeak/voices/en/en-rp.json'),
	de: () => import('mespeak/voices/de.json'),
	es: () => import('mespeak/voices/es.json'),
	'es-419': () => import('mespeak/voices/es-la.json'),
	fr: () => import('mespeak/voices/fr.json'),
	it: () => import('mespeak/voices/it.json'),
	nl: () => import('mespeak/voices/nl.json'),
	pl: () => import('mespeak/voices/pl.json'),
	pt: () => import('mespeak/voices/pt.json'),
	'pt-br': () => import('mespeak/voices/pt.json'),
	'pt-pt': () => import('mespeak/voices/pt-pt.json'),
	sv: () => import('mespeak/voices/sv.json'),
	fi: () => import('mespeak/voices/fi.json'),
	tr: () => import('mespeak/voices/tr.json'),
	cs: () => import('mespeak/voices/cs.json'),
	el: () => import('mespeak/voices/el.json'),
	hu: () => import('mespeak/voices/hu.json'),
	ro: () => import('mespeak/voices/ro.json'),
	sk: () => import('mespeak/voices/sk.json'),
	ca: () => import('mespeak/voices/ca.json'),
	lv: () => import('mespeak/voices/lv.json'),
	zh: () => import('mespeak/voices/zh.json'),
	'zh-yue': () => import('mespeak/voices/zh-yue.json'),
	eo: () => import('mespeak/voices/eo.json'),
	la: () => import('mespeak/voices/la.json'),
	kn: () => import('mespeak/voices/kn.json'),
};

const loadedVoiceIdsByKey = new Map<string, string>();
let activeSource: AudioBufferSourceNode | null = null;

export function resolveVoiceKey(locale: string | null | undefined): string {
	const normalized = (locale ?? '').replace(/_/g, '-').toLowerCase();
	if (!normalized) {
		return DEFAULT_VOICE_KEY;
	}
	if (VOICE_LOADERS[normalized]) {
		return normalized;
	}
	const language = normalized.split('-')[0];
	if (language && VOICE_LOADERS[language]) {
		return language;
	}
	return DEFAULT_VOICE_KEY;
}

export function rateToSpeed(rate: number | undefined): number {
	const multiplier = rate != null && Number.isFinite(rate) && rate > 0 ? rate : 1;
	const wpm = Math.round(ESPEAK_BASE_WPM * multiplier);
	return Math.min(ESPEAK_MAX_WPM, Math.max(ESPEAK_MIN_WPM, wpm));
}

function ensureConfig(): void {
	if (!meSpeak.isConfigLoaded()) {
		meSpeak.loadConfig(meSpeakConfig);
	}
}

async function ensureVoice(key: string): Promise<string> {
	const existing = loadedVoiceIdsByKey.get(key);
	if (existing) {
		return existing;
	}
	const loader = VOICE_LOADERS[key] ?? VOICE_LOADERS[DEFAULT_VOICE_KEY];
	const module = await loader();
	const data = module.default;
	const voiceId = (data as {voice_id?: string}).voice_id ?? key;
	meSpeak.loadVoice(data);
	loadedVoiceIdsByKey.set(key, voiceId);
	return voiceId;
}

export async function synthesize(
	text: string,
	options: {speed: number; voiceKey: string},
): Promise<ArrayBuffer | null> {
	ensureConfig();
	const voiceId = await ensureVoice(options.voiceKey);
	const result = meSpeak.speak(text, {rawdata: true, voice: voiceId, speed: options.speed});
	return result instanceof ArrayBuffer ? result : null;
}

export async function play(wav: ArrayBuffer, onEnded: () => void): Promise<{stop: () => void} | null> {
	let context: AudioContext;
	try {
		context = await SoundUtils.acquireOutputAudioContext();
	} catch (error) {
		logger.debug('Failed to acquire audio context for TTS fallback', error);
		onEnded();
		return null;
	}
	if (context.state === 'suspended') {
		onEnded();
		return null;
	}
	let audioBuffer: AudioBuffer;
	try {
		audioBuffer = await context.decodeAudioData(wav.slice(0));
	} catch (error) {
		logger.debug('Failed to decode TTS fallback audio', error);
		onEnded();
		return null;
	}
	const source = context.createBufferSource();
	source.buffer = audioBuffer;
	const gainNode = context.createGain();
	gainNode.gain.value = 1;
	source.connect(gainNode);
	gainNode.connect(context.destination);
	const cleanup = () => {
		if (activeSource === source) {
			activeSource = null;
		}
		try {
			source.disconnect();
		} catch {}
		try {
			gainNode.disconnect();
		} catch {}
	};
	source.onended = () => {
		cleanup();
		onEnded();
	};
	activeSource = source;
	source.start();
	return {
		stop: () => {
			source.onended = null;
			try {
				source.stop();
			} catch {}
			cleanup();
		},
	};
}

export function cancel(): void {
	if (!activeSource) {
		return;
	}
	const source = activeSource;
	activeSource = null;
	source.onended = null;
	try {
		source.stop();
	} catch {}
	try {
		source.disconnect();
	} catch {}
}
