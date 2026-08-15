// SPDX-License-Identifier: AGPL-3.0-or-later

import {highlightCodeInWorker} from '@app/features/code_highlighting/utils/ArboriumHighlightWorkerClient';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MAX_CODE_HIGHLIGHT_SOURCE_LENGTH} from '@fluxer/constants/src/LimitConstants';
import {useEffect, useMemo, useState} from 'react';

type ArboriumModule = typeof import('@arborium/arborium');

const logger = new Logger('ArboriumHighlighting');
const AUTO_DETECT_LANGUAGE_CODE = 'auto';
const PLAIN_TEXT_LANGUAGE = 'text';
const LANGUAGE_ALIAS_MAP: Record<string, string> = {
	adoc: 'asciidoc',
	ansible: 'yaml',
	bat: 'batch',
	cjs: 'javascript',
	cl: 'commonlisp',
	clj: 'clojure',
	cljc: 'clojure',
	cljs: 'clojure',
	cmd: 'batch',
	comp: 'glsl',
	conf: 'ini',
	config: 'ini',
	'c#': 'c-sharp',
	cs: 'c-sharp',
	csharp: 'c-sharp',
	'c++': 'cpp',
	cc: 'cpp',
	cfg: 'ini',
	cts: 'typescript',
	cxx: 'cpp',
	diff: 'diff',
	docker: 'dockerfile',
	dts: 'devicetree',
	dtsi: 'devicetree',
	ecmascript: 'javascript',
	erl: 'erlang',
	ex: 'elixir',
	exs: 'elixir',
	'f#': 'fsharp',
	frag: 'glsl',
	fs: 'fsharp',
	fsi: 'fsharp',
	fsx: 'fsharp',
	gql: 'graphql',
	graphqls: 'graphql',
	h: 'cpp',
	hrl: 'erlang',
	hs: 'haskell',
	hh: 'cpp',
	htm: 'html',
	hpp: 'cpp',
	hxx: 'cpp',
	j2: 'jinja2',
	jinja: 'jinja2',
	js: 'javascript',
	json5: 'json',
	jsonc: 'json',
	jl: 'julia',
	jsx: 'tsx',
	kt: 'kotlin',
	kts: 'kotlin',
	ksh: 'bash',
	lisp: 'commonlisp',
	log: 'text',
	md: 'markdown',
	mdown: 'markdown',
	mjs: 'javascript',
	mkdn: 'markdown',
	ml: 'ocaml',
	mli: 'ocaml',
	mm: 'objc',
	mts: 'typescript',
	nasm: 'x86asm',
	node: 'javascript',
	'obj-c': 'objc',
	'objective-c': 'objc',
	patch: 'diff',
	pbtxt: 'textproto',
	pgsql: 'sql',
	plain: 'text',
	plaintext: 'text',
	pl: 'perl',
	plist: 'xml',
	pm: 'perl',
	postgres: 'sql',
	postgresql: 'sql',
	proto: 'textproto',
	protobuf: 'textproto',
	ps: 'postscript',
	ps1: 'powershell',
	psd1: 'powershell',
	psm1: 'powershell',
	psql: 'sql',
	pwsh: 'powershell',
	py: 'python',
	py3: 'python',
	python3: 'python',
	rb: 'ruby',
	res: 'rescript',
	resi: 'rescript',
	rq: 'sparql',
	rs: 'rust',
	scm: 'scheme',
	sh: 'bash',
	shell: 'bash',
	shellscript: 'bash',
	ss: 'scheme',
	svg: 'xml',
	sv: 'verilog',
	svh: 'verilog',
	terraform: 'hcl',
	tf: 'hcl',
	tfvars: 'hcl',
	tla: 'tlaplus',
	ts: 'typescript',
	txt: 'text',
	typ: 'typst',
	vbnet: 'vb',
	vhd: 'vhdl',
	viml: 'vim',
	vimscript: 'vim',
	vert: 'glsl',
	xhtml: 'html',
	yml: 'yaml',
	zshell: 'zsh',
};
const MAX_CACHE_ENTRIES = 128;
const MAX_CACHED_SOURCE_LENGTH = 16 * 1024;
const MAX_CACHED_OUTPUT_LENGTH = 32 * 1024;
const MAX_CACHE_BYTES = 4 * 1024 * 1024;
const MAX_HIGHLIGHT_LANGUAGE_LENGTH = 128;

interface HighlightCacheEntry {
	promise: Promise<string | null>;
	retainedBytes: number;
	subscribers: number;
	settled: boolean;
	abortController: AbortController;
}

const highlightCache = new Map<string, HighlightCacheEntry>();
let highlightCacheBytes = 0;

let arboriumModule: ArboriumModule | null = null;
let arboriumPromise: Promise<ArboriumModule> | null = null;

function loadArborium(): Promise<ArboriumModule> {
	if (arboriumModule) {
		return Promise.resolve(arboriumModule);
	}
	if (!arboriumPromise) {
		arboriumPromise = (async () => {
			const [arborium] = await Promise.all([
				import('@arborium/arborium'),
				import('@arborium/arborium/themes/github-dark.css'),
				import('@arborium/arborium/themes/github-light.css'),
				import('@app/features/code_highlighting/utils/ArboriumThemeBridge.css'),
			]);
			arboriumModule = arborium;
			return arborium;
		})().catch((error) => {
			arboriumPromise = null;
			throw error;
		});
	}
	return arboriumPromise;
}

export async function _preloadArboriumForTests(): Promise<void> {
	await loadArborium();
}

export interface HighlightLanguageOption {
	canonicalCode: string;
	code: string;
}

export let HIGHLIGHT_LANGUAGE_OPTIONS: ReadonlyArray<HighlightLanguageOption> = [
	{canonicalCode: AUTO_DETECT_LANGUAGE_CODE, code: AUTO_DETECT_LANGUAGE_CODE},
	{canonicalCode: PLAIN_TEXT_LANGUAGE, code: PLAIN_TEXT_LANGUAGE},
	{canonicalCode: PLAIN_TEXT_LANGUAGE, code: 'plaintext'},
];

const optionsListeners = new Set<() => void>();

function notifyOptionsListeners(): void {
	for (const listener of optionsListeners) {
		listener();
	}
}

function buildHighlightLanguageOptions(arborium: ArboriumModule): Array<HighlightLanguageOption> {
	const optionsMap = new Map<string, HighlightLanguageOption>();
	optionsMap.set(AUTO_DETECT_LANGUAGE_CODE, {
		canonicalCode: AUTO_DETECT_LANGUAGE_CODE,
		code: AUTO_DETECT_LANGUAGE_CODE,
	});
	for (const canonicalCode of arborium.availableLanguages) {
		optionsMap.set(canonicalCode, {canonicalCode, code: canonicalCode});
	}
	for (const [aliasCode, canonicalCode] of Object.entries(LANGUAGE_ALIAS_MAP)) {
		const isCanonical =
			canonicalCode === PLAIN_TEXT_LANGUAGE ||
			(arborium.availableLanguages as ReadonlyArray<string>).includes(canonicalCode);
		if (!isCanonical || optionsMap.has(aliasCode)) {
			continue;
		}
		optionsMap.set(aliasCode, {canonicalCode, code: aliasCode});
	}
	if (!optionsMap.has('plaintext')) {
		optionsMap.set('plaintext', {canonicalCode: PLAIN_TEXT_LANGUAGE, code: 'plaintext'});
	}
	if (!optionsMap.has('text')) {
		optionsMap.set('text', {canonicalCode: PLAIN_TEXT_LANGUAGE, code: 'text'});
	}
	return Array.from(optionsMap.values()).sort((left, right) => left.code.localeCompare(right.code));
}

function ensureOptionsBuilt(): void {
	if (!arboriumModule) {
		void loadArborium()
			.then(() => {
				if (!arboriumModule) {
					return;
				}
				HIGHLIGHT_LANGUAGE_OPTIONS = buildHighlightLanguageOptions(arboriumModule);
				notifyOptionsListeners();
			})
			.catch((error) => logger.error('Failed to load Arborium language options', error));
		return;
	}
	if (HIGHLIGHT_LANGUAGE_OPTIONS.length <= 3) {
		HIGHLIGHT_LANGUAGE_OPTIONS = buildHighlightLanguageOptions(arboriumModule);
	}
}

export function useHighlightLanguageOptions(): ReadonlyArray<HighlightLanguageOption> {
	const [, setTick] = useState(0);
	useEffect(() => {
		ensureOptionsBuilt();
		const listener = () => setTick((tick) => tick + 1);
		optionsListeners.add(listener);
		return () => {
			optionsListeners.delete(listener);
		};
	}, []);
	return HIGHLIGHT_LANGUAGE_OPTIONS;
}

function getCacheKey(language: string, source: string): string {
	return `${language.length}:${language}\u0000${source}`;
}

function removeHighlightCacheEntry(cacheKey: string, entry: HighlightCacheEntry): void {
	if (highlightCache.get(cacheKey) !== entry) {
		return;
	}
	highlightCache.delete(cacheKey);
	highlightCacheBytes -= entry.retainedBytes;
	if (!entry.settled && entry.subscribers === 0) {
		entry.abortController.abort();
	}
}

function trimHighlightCache(): void {
	while (highlightCache.size > MAX_CACHE_ENTRIES || highlightCacheBytes > MAX_CACHE_BYTES) {
		const firstEntry = highlightCache.entries().next().value as [string, HighlightCacheEntry] | undefined;
		if (!firstEntry) {
			return;
		}
		removeHighlightCacheEntry(firstEntry[0], firstEntry[1]);
	}
}

function touchHighlightCacheEntry(cacheKey: string, entry: HighlightCacheEntry): void {
	highlightCache.delete(cacheKey);
	highlightCache.set(cacheKey, entry);
}

function releaseHighlightCacheConsumer(cacheKey: string, entry: HighlightCacheEntry): void {
	if (entry.subscribers <= 0) {
		throw new Error('Arborium highlight cache consumer count underflowed');
	}
	entry.subscribers -= 1;
	if (entry.subscribers === 0 && !entry.settled) {
		entry.abortController.abort();
		removeHighlightCacheEntry(cacheKey, entry);
	}
}

function subscribeToHighlightCacheEntry(cacheKey: string, entry: HighlightCacheEntry, signal?: AbortSignal): void {
	if (entry.settled) {
		return;
	}
	entry.subscribers++;
	let released = false;
	const release = (): void => {
		if (released) {
			return;
		}
		released = true;
		if (signal !== undefined) {
			signal.removeEventListener('abort', release);
		}
		releaseHighlightCacheConsumer(cacheKey, entry);
	};
	if (signal !== undefined) {
		signal.addEventListener('abort', release, {once: true});
		if (signal.aborted) {
			release();
			return;
		}
	}
	void entry.promise.then(release, release);
}

function isSignalAborted(signal: AbortSignal | undefined): boolean {
	if (signal === undefined) {
		return false;
	}
	return signal.aborted;
}

function loadHighlightedHtml(language: string, source: string, signal?: AbortSignal): Promise<string | null> {
	if (isSignalAborted(signal)) {
		return Promise.resolve(null);
	}
	if (source.length > MAX_CACHED_SOURCE_LENGTH) {
		if (signal === undefined) {
			return highlightCodeInWorker(language, source);
		}
		return highlightCodeInWorker(language, source, {signal});
	}
	const cacheKey = getCacheKey(language, source);
	const cachedEntry = highlightCache.get(cacheKey);
	if (cachedEntry) {
		touchHighlightCacheEntry(cacheKey, cachedEntry);
		subscribeToHighlightCacheEntry(cacheKey, cachedEntry, signal);
		return cachedEntry.promise;
	}
	const entry: HighlightCacheEntry = {
		promise: Promise.resolve(null),
		retainedBytes: source.length * 2,
		subscribers: 0,
		settled: false,
		abortController: new AbortController(),
	};
	const highlightedHtmlPromise = highlightCodeInWorker(language, source, {signal: entry.abortController.signal})
		.then((highlightedHtml) => {
			entry.settled = true;
			if (highlightCache.get(cacheKey) !== entry) {
				return highlightedHtml;
			}
			if (highlightedHtml === null || highlightedHtml.length > MAX_CACHED_OUTPUT_LENGTH) {
				removeHighlightCacheEntry(cacheKey, entry);
				return highlightedHtml;
			}
			const nextRetainedBytes = (source.length + highlightedHtml.length) * 2;
			highlightCacheBytes += nextRetainedBytes - entry.retainedBytes;
			entry.retainedBytes = nextRetainedBytes;
			trimHighlightCache();
			return highlightedHtml;
		})
		.catch((error) => {
			entry.settled = true;
			removeHighlightCacheEntry(cacheKey, entry);
			throw error;
		});
	entry.promise = highlightedHtmlPromise;
	highlightCache.set(cacheKey, entry);
	highlightCacheBytes += entry.retainedBytes;
	subscribeToHighlightCacheEntry(cacheKey, entry, signal);
	trimHighlightCache();
	return highlightedHtmlPromise;
}

function getLanguageToken(language?: string | null): string | null {
	const trimmedLanguage = language?.trim();
	if (!trimmedLanguage) {
		return null;
	}
	const [primaryLanguage] = trimmedLanguage.split(/\s+/u);
	return primaryLanguage ? primaryLanguage.toLowerCase() : null;
}

export function escapeCodeHtml(value: string): string {
	return value.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

export function normalizeHighlightLanguage(language?: string | null): string | null {
	const languageToken = getLanguageToken(language);
	if (!languageToken || languageToken.length > MAX_HIGHLIGHT_LANGUAGE_LENGTH) {
		return null;
	}
	const aliasedLanguage = LANGUAGE_ALIAS_MAP[languageToken];
	if (!arboriumModule) {
		const candidate = aliasedLanguage ?? languageToken;
		if (candidate === PLAIN_TEXT_LANGUAGE) {
			return PLAIN_TEXT_LANGUAGE;
		}
		return candidate;
	}
	const normalizedLanguage = arboriumModule.normalizeLanguage(languageToken);
	const canonicalLanguage = aliasedLanguage ?? LANGUAGE_ALIAS_MAP[normalizedLanguage] ?? normalizedLanguage;
	if (canonicalLanguage === PLAIN_TEXT_LANGUAGE) {
		return PLAIN_TEXT_LANGUAGE;
	}
	return (arboriumModule.availableLanguages as ReadonlyArray<string>).includes(canonicalLanguage)
		? canonicalLanguage
		: null;
}

export function isSupportedHighlightLanguage(language?: string | null): boolean {
	if (getLanguageToken(language) === AUTO_DETECT_LANGUAGE_CODE) {
		return true;
	}
	return normalizeHighlightLanguage(language) !== null;
}

function resolveHighlightLanguage(language?: string | null): string | null {
	const languageToken = getLanguageToken(language);
	if (!languageToken) {
		return null;
	}
	if (languageToken === AUTO_DETECT_LANGUAGE_CODE) {
		return AUTO_DETECT_LANGUAGE_CODE;
	}
	return normalizeHighlightLanguage(languageToken);
}

export async function highlightCodeHtml(
	language?: string | null,
	source?: string | null,
	signal?: AbortSignal,
): Promise<string> {
	if (!source) {
		return '';
	}
	if (source.length >= MAX_CODE_HIGHLIGHT_SOURCE_LENGTH) {
		return escapeCodeHtml(source);
	}
	try {
		const languageToken = getLanguageToken(language);
		if (!languageToken || languageToken === PLAIN_TEXT_LANGUAGE) {
			return escapeCodeHtml(source);
		}
		if (languageToken.length > MAX_HIGHLIGHT_LANGUAGE_LENGTH) {
			return escapeCodeHtml(source);
		}
		if (languageToken !== AUTO_DETECT_LANGUAGE_CODE) {
			await loadArborium();
		}
		const resolvedLanguage = resolveHighlightLanguage(language);
		if (!resolvedLanguage || resolvedLanguage === PLAIN_TEXT_LANGUAGE) {
			return escapeCodeHtml(source);
		}
		const highlightedHtml = await loadHighlightedHtml(resolvedLanguage, source, signal);
		return highlightedHtml === null ? escapeCodeHtml(source) : highlightedHtml;
	} catch (error) {
		const languageForLog = language == null ? '' : language;
		logger.error(`Failed to highlight code with Arborium for language "${languageForLog}"`, error);
		return escapeCodeHtml(source);
	}
}

export function useArboriumHighlightedHtml(language?: string | null, source?: string | null): string {
	const escapedHtml = useMemo(() => escapeCodeHtml(source ?? ''), [source]);
	const [highlightedHtml, setHighlightedHtml] = useState(escapedHtml);
	useEffect(() => {
		let cancelled = false;
		const controller = new AbortController();
		setHighlightedHtml(escapedHtml);
		if (!source) {
			controller.abort();
			return () => {
				cancelled = true;
			};
		}
		void highlightCodeHtml(language, source, controller.signal).then((html) => {
			if (!cancelled) {
				setHighlightedHtml(html);
			}
		});
		return () => {
			cancelled = true;
			controller.abort();
		};
	}, [escapedHtml, language, source]);
	return highlightedHtml;
}
