// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';

const logger = new Logger('ScriptFontLoader');

export type ScriptChunk = 'non-latin' | 'sc' | 'tc' | 'jp' | 'kr';

const CHUNK_IMPORTS: Record<ScriptChunk, () => Promise<unknown>> = {
	'non-latin': () => import('@app/features/theme/fonts/ScriptFacesNonLatin'),
	sc: () => import('@app/features/theme/fonts/ScriptFacesSC'),
	tc: () => import('@app/features/theme/fonts/ScriptFacesTC'),
	jp: () => import('@app/features/theme/fonts/ScriptFacesJP'),
	kr: () => import('@app/features/theme/fonts/ScriptFacesKR'),
};

const KANA = /[\u3040-\u30FF\u31F0-\u31FF\uFF66-\uFF9D]/;

const HANGUL = /[\u1100-\u11FF\u3130-\u318F\uA960-\uA97F\uAC00-\uD7FF]/;

const HAN = /[\u3400-\u4DBF\u4E00-\u9FFF\uF900-\uFAFF]|[\uD840-\uD87F][\uDC00-\uDFFF]/;

const CJK_ADJACENT = /[\u2E80-\u2EFF\u2F00-\u2FDF\u2FF0-\u303F\u3190-\u31EF\u3200-\u33FF\uFF00-\uFF65\uFF9E-\uFFEF]/;

const requested = new Set<ScriptChunk>();
const inFlight = new Set<Promise<void>>();

function request(chunk: ScriptChunk): void {
	if (requested.has(chunk)) return;
	requested.add(chunk);
	const load = loadLazyModule(CHUNK_IMPORTS[chunk])
		.then(() => undefined)
		.catch((error: unknown) => {
			logger.warn(`Failed to load the ${chunk} font faces; falling back to OS script fonts:`, error);
		})
		.finally(() => {
			inFlight.delete(load);
		});
	inFlight.add(load);
}

export function hanChunkForLanguage(language: string | null | undefined): ScriptChunk {
	const tag = (language ?? '').toLowerCase();
	if (tag === 'ja' || tag.startsWith('ja-')) return 'jp';
	if (tag.startsWith('zh')) {
		if (/(^|-)(tw|hk|mo|hant)(-|$)/.test(tag)) return 'tc';
		return 'sc';
	}
	return 'sc';
}

function bootChunkForLanguage(language: string | null | undefined): ScriptChunk | null {
	const tag = (language ?? '').toLowerCase();
	if (tag === 'ja' || tag.startsWith('ja-')) return 'jp';
	if (tag === 'ko' || tag.startsWith('ko-')) return 'kr';
	if (tag.startsWith('zh')) return hanChunkForLanguage(tag);
	return null;
}

function documentLanguage(): string | null {
	if (typeof document === 'undefined') return null;
	return document.documentElement.lang || null;
}

export function noteText(text: string | null | undefined): void {
	if (!text) return;
	if (requested.has('jp') && requested.has('kr') && requested.has('sc') && requested.has('tc')) return;
	if (KANA.test(text)) request('jp');
	if (HANGUL.test(text)) request('kr');
	if (HAN.test(text) || CJK_ADJACENT.test(text)) request(hanChunkForLanguage(documentLanguage()));
}

export function noteLocale(language: string | null | undefined = documentLanguage()): void {
	const chunk = bootChunkForLanguage(language);
	if (chunk) request(chunk);
}

export function scheduleNonLatinScriptFaces(): void {
	const run = () => request('non-latin');
	if (typeof requestIdleCallback === 'function') {
		requestIdleCallback(run, {timeout: 3000});
		return;
	}
	setTimeout(run, 1000);
}

export function requestedScriptChunks(): ReadonlySet<ScriptChunk> {
	return requested;
}

export function resetScriptFontLoaderForTests(): void {
	requested.clear();
}

export async function whenScriptChunksSettled(): Promise<void> {
	while (inFlight.size > 0) {
		await Promise.all([...inFlight]);
	}
}
