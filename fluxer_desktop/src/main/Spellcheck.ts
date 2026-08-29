// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import * as fsSync from 'node:fs';
import * as fs from 'node:fs/promises';
import {createRequire} from 'node:module';
import * as path from 'node:path';
import {STATIC_CDN_URL} from '@electron/common/Constants';
import type {SpellcheckBundledDictionary} from '@electron/common/Types';
import {getSpellcheckLaunchMode, type SpellcheckLaunchMode} from '@electron/main/LaunchOptions';
import {getNativeLocale} from '@electron/main/MainI18n';
import {
	BUNDLED_DICTIONARIES,
	DICTIONARY_SOURCES,
	type SpellcheckDictionaryCatalogEntry,
} from '@electron/main/SpellcheckDictionaries';
import {app, ipcMain, type Session, type WebContents} from 'electron';
import log from 'electron-log';
import {franc} from 'franc-min';
import {type Hunspell, type HunspellFactory, loadModule} from 'hunspell-asm';

const requireModule = createRequire(import.meta.url);

type SystemHunspellNative = {
	Hunspell:
		| (new (
				affPath: string,
				dicPath: string,
		  ) => {
				spell(word: string): boolean;
				suggest(word: string, max?: number): Array<string>;
				add(word: string): void;
				remove(word: string): void;
				close(): void;
		  })
		| null;
	discoverSystemDictionaries: (() => Array<{tag: string; affPath: string; dicPath: string}>) | null;
	hashFile: ((path: string) => Promise<string>) | null;
	loadError: Error | null;
};

let systemHunspellModule: SystemHunspellNative | null = null;

function loadSystemHunspell(): SystemHunspellNative | null {
	if (process.platform !== 'linux') return null;
	if (systemHunspellModule) return systemHunspellModule;
	try {
		const mod = requireModule('@fluxer/system-hunspell') as SystemHunspellNative;
		if (!mod.Hunspell || !mod.discoverSystemDictionaries) {
			throw mod.loadError ?? new Error('@fluxer/system-hunspell exports missing');
		}
		systemHunspellModule = mod;
		return mod;
	} catch (error) {
		log.warn('[Spellcheck] @fluxer/system-hunspell unavailable; falling back to hunspell-asm on Linux', {error});
		return null;
	}
}

type SpellcheckEngine = 'auto' | 'hunspell' | 'system';

interface SpellcheckState {
	enabled: boolean;
	engine: SpellcheckEngine;
	autoDetect: boolean;
	languages: Array<string>;
	personalDictionary: Array<string>;
}

interface RendererSpellcheckPatch {
	enabled?: boolean;
	engine?: SpellcheckEngine;
	autoDetect?: boolean;
	languages?: Array<string>;
	personalDictionary?: Array<string>;
}

interface SpellcheckDictionaryData {
	aff: Uint8Array;
	dic: Uint8Array;
}

interface ResolvedEngine {
	mode: 'hunspell' | 'system' | 'off';
	hunspellLangs: Array<string>;
	systemLangs: Array<string>;
	allowSystemHunspell?: boolean;
	preferSystemHunspell?: boolean;
}

interface LoadedHunspell {
	tag: string;
	hunspell: Hunspell;
	affPath: string;
	dicPath: string;
}

const STATIC_CDN_ENDPOINT = process.env.FLUXER_STATIC_CDN_ENDPOINT?.replace(/\/+$/, '') ?? '';
const DICTIONARY_DOWNLOAD_BASE_URL =
	process.env.FLUXER_SPELLCHECK_DICTIONARY_BASE_URL ||
	`${STATIC_CDN_ENDPOINT || STATIC_CDN_URL}/desktop/spellcheck/dictionaries`;
const DICTIONARY_CACHE_VERSION = 1;
const DICTIONARY_DOWNLOAD_TIMEOUT_MS = 15000;
const DICTIONARY_RETRY_DELAYS_MS: ReadonlyArray<number> = [5000, 15000, 45000];
const AUTODETECT_MAX_TEXT_LENGTH = 2000;
const AUTODETECT_PREFIX_SKIP_CHARS = 20;
const AUTODETECT_CONTEXT_MAX_LENGTH = 128;
const BUILTIN_ALLOWLIST: ReadonlyArray<string> = ['Fluxer', 'fluxer', 'Hampus'];
const isLinux = process.platform === 'linux';
const isMac = process.platform === 'darwin';
const useChromiumSpellchecker = !isLinux;
const defaultState: SpellcheckState = {
	enabled: true,
	engine: 'auto',
	autoDetect: true,
	languages: [],
	personalDictionary: [],
};
const sessionState = new WeakMap<Session, SpellcheckState>();
const sessionResolvedEngine = new WeakMap<Session, ResolvedEngine>();
const sessionLoaded = new WeakMap<Session, Map<string, LoadedHunspell>>();
const sessionPersonalApplied = new WeakMap<Session, Set<string>>();
const sessionApplyGeneration = new WeakMap<Session, number>();
const sessionAutoDetectedLanguage = new WeakMap<Session, string | null>();
const autodetectCacheByWebContents = new WeakMap<
	WebContents,
	Map<
		string,
		{
			text: string;
			language: string | null;
		}
	>
>();
const sessionRetryTimer = new WeakMap<Session, NodeJS.Timeout>();
const sessionRetryAttempt = new WeakMap<Session, number>();
const dictionaryDataPromises = new Map<string, Promise<SpellcheckDictionaryData | null>>();

let factoryPromise: Promise<HunspellFactory> | null = null;
let ipcRegistered = false;
let launchModeLogged = false;

const contextSourceByWebContents = new WeakMap<
	WebContents,
	{
		isTextarea: boolean;
		ts: number;
	}
>();
const norm = (code: string): string => code.toLowerCase();
const beginSessionApply = (session: Session): number => {
	const generation = (sessionApplyGeneration.get(session) ?? 0) + 1;
	sessionApplyGeneration.set(session, generation);
	return generation;
};
const isSessionApplyCurrent = (session: Session, generation: number): boolean =>
	sessionApplyGeneration.get(session) === generation;
const dictionaryByTag = new Map<string, SpellcheckDictionaryCatalogEntry>(
	BUNDLED_DICTIONARIES.map((d) => [norm(d.tag), d]),
);
const resolveBundledFor = (tag: string): SpellcheckDictionaryCatalogEntry | null => {
	const lower = norm(tag);
	const exact = dictionaryByTag.get(lower);
	if (exact) return exact;
	const base = lower.split('-')[0];
	return dictionaryByTag.get(base) ?? null;
};
const listBundled = (): Array<SpellcheckBundledDictionary> =>
	BUNDLED_DICTIONARIES.map((dict) => ({
		tag: dict.tag,
		package: dict.package,
		displayName: dict.displayName,
		nativeName: dict.nativeName,
	}));
const ensureFactory = (): Promise<HunspellFactory> => {
	if (!factoryPromise) {
		factoryPromise = loadModule().catch((error) => {
			factoryPromise = null;
			log.error('[Spellcheck] Failed to load hunspell-asm', error);
			throw error;
		});
	}
	return factoryPromise;
};
const dictionaryCacheKey = (dict: SpellcheckDictionaryCatalogEntry): string =>
	`${dict.package}@${DICTIONARY_SOURCES[dict.package].version}`;
const dictionaryCacheDir = (dict: SpellcheckDictionaryCatalogEntry): string =>
	path.join(
		app.getPath('userData'),
		'spellcheck-dictionaries',
		`v${DICTIONARY_CACHE_VERSION}`,
		dictionaryCacheKey(dict),
	);

export function cleanupLinuxChromiumSpellcheckDictionaries(userDataPath: string): void {
	if (!isLinux) return;
	const chromiumDictionaryDir = path.join(userDataPath, 'Dictionaries');
	if (!fsSync.existsSync(chromiumDictionaryDir)) return;
	try {
		fsSync.rmSync(chromiumDictionaryDir, {recursive: true, force: true});
		log.info('[Spellcheck] Removed Chromium spellcheck dictionary cache on Linux', {path: chromiumDictionaryDir});
	} catch (error) {
		log.warn('[Spellcheck] Failed to remove Chromium spellcheck dictionary cache on Linux', {
			path: chromiumDictionaryDir,
			error,
		});
	}
}

const dictionaryFileUrl = (dict: SpellcheckDictionaryCatalogEntry, fileName: 'index.aff' | 'index.dic'): string => {
	if (!DICTIONARY_DOWNLOAD_BASE_URL) {
		throw new Error('Set FLUXER_SPELLCHECK_DICTIONARY_BASE_URL or FLUXER_STATIC_CDN_ENDPOINT to download dictionaries');
	}
	const base = DICTIONARY_DOWNLOAD_BASE_URL.replace(/\/+$/, '');
	return `${base}/${dict.package}@${DICTIONARY_SOURCES[dict.package].version}/${fileName}`;
};
const sha256 = (data: Uint8Array): string => createHash('sha256').update(data).digest('hex');
const verifyDictionaryFile = (
	dict: SpellcheckDictionaryCatalogEntry,
	fileName: 'index.aff' | 'index.dic',
	data: Uint8Array,
): boolean => {
	const source = DICTIONARY_SOURCES[dict.package];
	const expectedBytes = fileName === 'index.aff' ? source.affBytes : source.dicBytes;
	const expectedSha256 = fileName === 'index.aff' ? source.affSha256 : source.dicSha256;
	return data.byteLength === expectedBytes && sha256(data) === expectedSha256;
};
const sha256OfPath = async (filePath: string): Promise<string> => {
	return sha256(await fs.readFile(filePath));
};
const readCachedDictionaryFile = async (
	dict: SpellcheckDictionaryCatalogEntry,
	fileName: 'index.aff' | 'index.dic',
): Promise<Buffer | null> => {
	const filePath = path.join(dictionaryCacheDir(dict), fileName);
	try {
		const stat = await fs.stat(filePath);
		const source = DICTIONARY_SOURCES[dict.package];
		const expectedBytes = fileName === 'index.aff' ? source.affBytes : source.dicBytes;
		const expectedSha256 = fileName === 'index.aff' ? source.affSha256 : source.dicSha256;
		if (stat.size !== expectedBytes) {
			await fs.rm(filePath, {force: true});
			log.warn(`[Spellcheck] Removed size-mismatched cached dictionary file ${dictionaryCacheKey(dict)}/${fileName}`);
			return null;
		}
		if ((await sha256OfPath(filePath)) !== expectedSha256) {
			await fs.rm(filePath, {force: true});
			log.warn(`[Spellcheck] Removed hash-mismatched cached dictionary file ${dictionaryCacheKey(dict)}/${fileName}`);
			return null;
		}
		return await fs.readFile(filePath);
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
			return null;
		}
		throw error;
	}
};
const writeCachedDictionaryFile = async (
	dict: SpellcheckDictionaryCatalogEntry,
	fileName: 'index.aff' | 'index.dic',
	data: Uint8Array,
): Promise<void> => {
	const dir = dictionaryCacheDir(dict);
	const filePath = path.join(dir, fileName);
	const tmpPath = path.join(dir, `.${fileName}.${process.pid}.${Date.now()}.tmp`);
	await fs.mkdir(dir, {recursive: true});
	await fs.writeFile(tmpPath, data, {mode: 0o600});
	try {
		await fs.rename(tmpPath, filePath);
	} catch (error) {
		await fs.rm(tmpPath, {force: true}).catch(() => {});
		throw error;
	}
};
const downloadDictionaryFile = async (
	dict: SpellcheckDictionaryCatalogEntry,
	fileName: 'index.aff' | 'index.dic',
): Promise<Buffer> => {
	const controller = new AbortController();
	const timeout = setTimeout(() => controller.abort(), DICTIONARY_DOWNLOAD_TIMEOUT_MS);
	try {
		const response = await fetch(dictionaryFileUrl(dict, fileName), {
			headers: {
				Accept: 'application/octet-stream,*/*;q=0.8',
			},
			signal: controller.signal,
		});
		if (!response.ok) {
			throw new Error(`HTTP ${response.status}`);
		}
		const data = Buffer.from(await response.arrayBuffer());
		if (!verifyDictionaryFile(dict, fileName, data)) {
			throw new Error(`sha256/size mismatch for ${dictionaryCacheKey(dict)}/${fileName}`);
		}
		await writeCachedDictionaryFile(dict, fileName, data);
		return data;
	} finally {
		clearTimeout(timeout);
	}
};
const loadLocalDictionaryPackage = async (
	dict: SpellcheckDictionaryCatalogEntry,
): Promise<SpellcheckDictionaryData | null> => {
	try {
		const mod = await opaqueImport(dict.package);
		const data = {
			aff: Buffer.from(mod.default.aff),
			dic: Buffer.from(mod.default.dic),
		};
		if (!verifyDictionaryFile(dict, 'index.aff', data.aff) || !verifyDictionaryFile(dict, 'index.dic', data.dic)) {
			log.warn(`[Spellcheck] Ignoring local dictionary package with unexpected contents: ${dictionaryCacheKey(dict)}`);
			return null;
		}
		return data;
	} catch {
		return null;
	}
};
const loadLazyDictionaryData = async (
	dict: SpellcheckDictionaryCatalogEntry,
): Promise<SpellcheckDictionaryData | null> => {
	const key = dictionaryCacheKey(dict);
	const existing = dictionaryDataPromises.get(key);
	if (existing) return existing;
	const promise = (async () => {
		const cachedAff = await readCachedDictionaryFile(dict, 'index.aff');
		const cachedDic = await readCachedDictionaryFile(dict, 'index.dic');
		if (cachedAff && cachedDic) {
			return {aff: cachedAff, dic: cachedDic};
		}
		const local = await loadLocalDictionaryPackage(dict);
		if (local) return local;
		const [aff, dic] = await Promise.all([
			cachedAff ?? downloadDictionaryFile(dict, 'index.aff'),
			cachedDic ?? downloadDictionaryFile(dict, 'index.dic'),
		]);
		return {aff, dic};
	})()
		.catch((error) => {
			log.warn(`[Spellcheck] Failed to lazily load dictionary ${key}`, error);
			return null;
		})
		.finally(() => {
			dictionaryDataPromises.delete(key);
		});
	dictionaryDataPromises.set(key, promise);
	return promise;
};
const opaqueImport = new Function('specifier', 'return import(specifier)') as (specifier: string) => Promise<{
	default: {
		aff: Uint8Array;
		dic: Uint8Array;
	};
}>;
const loadHunspellForTag = async (
	tag: string,
	shouldContinue: () => boolean,
	allowSystemFallback: boolean,
	preferSystem: boolean,
): Promise<LoadedHunspell | null> => {
	if (preferSystem) {
		const native = loadFromSystemHunspell(tag);
		if (native) return native;
	}
	const dict = resolveBundledFor(tag);
	if (dict) {
		try {
			const data = await loadLazyDictionaryData(dict);
			if (!shouldContinue()) return null;
			if (data) {
				const factory = await ensureFactory();
				if (!shouldContinue()) return null;
				const {aff, dic} = data;
				const affPath = factory.mountBuffer(aff, `${dict.tag}.aff`);
				const dicPath = factory.mountBuffer(dic, `${dict.tag}.dic`);
				const hunspell = factory.create(affPath, dicPath);
				for (const word of BUILTIN_ALLOWLIST) {
					hunspell.addWord(word);
				}
				log.info(`[Spellcheck] Loaded bundled hunspell dictionary for "${tag}" from ${dict.package}`);
				return {tag: dict.tag, hunspell, affPath, dicPath};
			}
		} catch (error) {
			log.warn(`[Spellcheck] Failed to load Hunspell dictionary "${dict.package}"`, error);
		}
		if (isLinux) {
			return allowSystemFallback ? loadFromSystemHunspell(tag) : null;
		}
	}
	return allowSystemFallback || !isLinux ? loadFromSystemHunspell(tag) : null;
};
const NATIVE_AFF_PATH_TAG = '__native__';

function loadFromSystemHunspell(tag: string): LoadedHunspell | null {
	const mod = loadSystemHunspell();
	if (!mod || !mod.Hunspell || !mod.discoverSystemDictionaries) return null;
	const wantedKey = norm(tag);
	const wantedBase = languageBase(tag);
	const installed = mod.discoverSystemDictionaries();
	const match =
		installed.find((entry) => norm(entry.tag) === wantedKey) ??
		installed.find((entry) => languageBase(entry.tag) === wantedBase);
	if (!match) return null;
	try {
		const native = new mod.Hunspell(match.affPath, match.dicPath);
		for (const word of BUILTIN_ALLOWLIST) {
			try {
				native.add(word);
			} catch {}
		}
		const adapter = {
			spell(word: string) {
				return native.spell(word);
			},
			suggest(word: string) {
				return native.suggest(word, 12);
			},
			addWord(word: string) {
				native.add(word);
			},
			removeWord(word: string) {
				native.remove(word);
			},
			dispose() {
				native.close();
			},
		} as unknown as Hunspell;
		log.info(`[Spellcheck] Loaded system hunspell dictionary for "${tag}" from ${match.affPath}`);
		return {tag, hunspell: adapter, affPath: NATIVE_AFF_PATH_TAG, dicPath: NATIVE_AFF_PATH_TAG};
	} catch (error) {
		log.warn(`[Spellcheck] Native hunspell failed for "${tag}"; will retry hunspell-asm`, {error});
		return null;
	}
}

const isNativeLoaded = (loaded: LoadedHunspell): boolean => loaded.affPath === NATIVE_AFF_PATH_TAG;
const disposeLoaded = (loaded: LoadedHunspell, factory: HunspellFactory | null): void => {
	try {
		loaded.hunspell.dispose();
	} catch (error) {
		log.warn('[Spellcheck] dispose failed', error);
	}
	if (isNativeLoaded(loaded)) return;
	if (!factory) {
		log.warn('[Spellcheck] Hunspell WASM dictionary disposed without unmounting buffers');
		return;
	}
	try {
		factory.unmount(loaded.affPath);
	} catch {}
	try {
		factory.unmount(loaded.dicPath);
	} catch {}
};
const detectPreferredLanguages = (session: Session): Array<string> => {
	const electronAvailable = useChromiumSpellchecker ? (session.availableSpellCheckerLanguages ?? []) : [];
	const fromOs = (typeof app.getPreferredSystemLanguages === 'function' && app.getPreferredSystemLanguages()) || [];
	const preferred = [getNativeLocale(), ...fromOs, app.getLocale()];
	const seen = new Set<string>();
	const result: Array<string> = [];
	for (const raw of preferred) {
		const lower = norm(raw);
		if (seen.has(lower)) continue;
		seen.add(lower);
		const exact = electronAvailable.find((c) => norm(c) === lower);
		if (exact) {
			result.push(exact);
		} else {
			result.push(raw);
		}
	}
	return result;
};
const ISO6393_TO_DICTIONARY_BASE: Record<string, string> = {
	bul: 'bg',
	cat: 'ca',
	ces: 'cs',
	dan: 'da',
	deu: 'de',
	ell: 'el',
	eng: 'en',
	epo: 'eo',
	est: 'et',
	eus: 'eu',
	fas: 'fa',
	fra: 'fr',
	glg: 'gl',
	heb: 'he',
	hrv: 'hr',
	hun: 'hu',
	isl: 'is',
	ita: 'it',
	kor: 'ko',
	lav: 'lv',
	lit: 'lt',
	nld: 'nl',
	nob: 'nb',
	pol: 'pl',
	por: 'pt',
	ron: 'ro',
	rus: 'ru',
	slk: 'sk',
	slv: 'sl',
	spa: 'es',
	srp: 'sr',
	swe: 'sv',
	tur: 'tr',
	ukr: 'uk',
	vie: 'vi',
};
const AUTODETECT_SUPPORTED_ISO6393 = Object.keys(ISO6393_TO_DICTIONARY_BASE);
const languageBase = (tag: string): string => norm(tag).split('-')[0];
const chooseBundledTagForLanguage = (language: string, session: Session): string | null => {
	const lower = languageBase(language);
	if (lower === 'en') {
		for (const preferred of detectPreferredLanguages(session)) {
			const dict = resolveBundledFor(preferred);
			if (dict && norm(dict.tag) === 'en-gb') {
				return dict.tag;
			}
		}
		return resolveBundledFor('en-US')?.tag ?? resolveBundledFor('en')?.tag ?? null;
	}
	for (const preferred of detectPreferredLanguages(session)) {
		const dict = resolveBundledFor(preferred);
		if (dict && languageBase(dict.tag) === lower) {
			return dict.tag;
		}
	}
	return resolveBundledFor(lower)?.tag ?? null;
};
const detectLanguageFromText = (text: string, session: Session): string | null => {
	const code = franc(text.slice(0, AUTODETECT_MAX_TEXT_LENGTH), {
		minLength: 3,
		only: AUTODETECT_SUPPORTED_ISO6393,
	});
	const dictionaryBase = ISO6393_TO_DICTIONARY_BASE[code];
	if (!dictionaryBase) return null;
	return chooseBundledTagForLanguage(dictionaryBase, session);
};
const isTinyAutodetectPrefixChange = (previous: string, next: string): boolean => {
	if (previous.length === 0 || next.length === 0) return false;
	if (Math.abs(next.length - previous.length) >= AUTODETECT_PREFIX_SKIP_CHARS) return false;
	return next.startsWith(previous) || previous.startsWith(next);
};
const getAutodetectContextKey = (value: unknown): string => {
	if (typeof value !== 'string') return 'default';
	const trimmed = value.trim();
	if (trimmed.length === 0) return 'default';
	return trimmed.slice(0, AUTODETECT_CONTEXT_MAX_LENGTH);
};
const detectLanguageFromContextText = (webContents: WebContents, contextKey: string, text: string): string | null => {
	let cache = autodetectCacheByWebContents.get(webContents);
	if (!cache) {
		cache = new Map();
		autodetectCacheByWebContents.set(webContents, cache);
	}
	const previous = cache.get(contextKey);
	if (previous && (previous.text === text || isTinyAutodetectPrefixChange(previous.text, text))) {
		return previous.language;
	}
	const language = text.trim().length === 0 ? null : detectLanguageFromText(text, webContents.session);
	cache.set(contextKey, {text, language});
	return language;
};
const sanitizeEngine = (value: unknown): SpellcheckEngine => {
	if (value === 'hunspell' || value === 'system' || value === 'auto') return value;
	return defaultState.engine;
};
const normalizeState = (prev: SpellcheckState, patch: RendererSpellcheckPatch | SpellcheckState): SpellcheckState => {
	const next: SpellcheckState = {
		enabled: patch.enabled ?? prev.enabled,
		engine: sanitizeEngine(patch.engine ?? prev.engine),
		autoDetect: patch.autoDetect ?? prev.autoDetect,
		languages: Array.isArray(patch.languages) ? [...patch.languages] : [...prev.languages],
		personalDictionary: Array.isArray(patch.personalDictionary)
			? [...patch.personalDictionary]
			: [...prev.personalDictionary],
	};
	const seen = new Set<string>();
	next.languages = next.languages.filter((tag) => {
		if (typeof tag !== 'string' || tag.length === 0) return false;
		const key = norm(tag);
		if (seen.has(key)) return false;
		seen.add(key);
		return true;
	});
	const seenWords = new Set<string>();
	next.personalDictionary = next.personalDictionary
		.map((w) => (typeof w === 'string' ? w.trim() : ''))
		.filter((w) => {
			if (w.length === 0) return false;
			if (seenWords.has(w)) return false;
			seenWords.add(w);
			return true;
		});
	return next;
};
const applyLaunchSpellcheckMode = (state: SpellcheckState): SpellcheckState => {
	const mode: SpellcheckLaunchMode = getSpellcheckLaunchMode(process.argv);
	if (mode === 'default') return state;
	if (mode === 'off') {
		return {...state, enabled: false};
	}
	return {...state, enabled: true, engine: mode};
};
const requestedLanguages = (state: SpellcheckState, session: Session): Array<string> => {
	if (!state.autoDetect) return state.languages;
	const detected = sessionAutoDetectedLanguage.get(session);
	const seen = new Set<string>();
	const requested: Array<string> = [];
	for (const tag of detected ? [detected, ...detectPreferredLanguages(session)] : detectPreferredLanguages(session)) {
		const key = norm(tag);
		if (seen.has(key)) continue;
		seen.add(key);
		requested.push(tag);
	}
	return requested;
};
const resolveSystemLanguages = (session: Session, requested: Array<string>): Array<string> => {
	const sysOnly: Array<string> = [];
	const seen = new Set<string>();
	for (const tag of requested) {
		const lower = norm(tag);
		const match =
			(session.availableSpellCheckerLanguages ?? []).find((c) => norm(c) === lower) ||
			(session.availableSpellCheckerLanguages ?? []).find((c) => norm(c).split('-')[0] === lower.split('-')[0]) ||
			null;
		if (match && !seen.has(norm(match))) {
			seen.add(norm(match));
			sysOnly.push(match);
		}
	}
	return sysOnly;
};
const resolveSystemFallback = (state: SpellcheckState, session: Session): ResolvedEngine => ({
	mode: 'system',
	hunspellLangs: [],
	systemLangs: resolveSystemLanguages(session, requestedLanguages(state, session)),
});
const resolveEngine = (state: SpellcheckState, session: Session): ResolvedEngine => {
	if (!state.enabled) {
		return {mode: 'off', hunspellLangs: [], systemLangs: []};
	}
	const requested = requestedLanguages(state, session);
	const bundled: Array<string> = [];
	const seenBundled = new Set<string>();
	for (const tag of requested) {
		const dict = resolveBundledFor(tag);
		if (dict) {
			const key = norm(dict.tag);
			if (!seenBundled.has(key)) {
				seenBundled.add(key);
				bundled.push(dict.tag);
			}
		}
	}
	if (state.engine === 'hunspell') {
		return {mode: 'hunspell', hunspellLangs: bundled, systemLangs: []};
	}
	if (!isLinux) {
		return {mode: 'system', hunspellLangs: [], systemLangs: resolveSystemLanguages(session, requested)};
	}
	return {
		mode: 'hunspell',
		hunspellLangs: bundled.length > 0 ? bundled : requested,
		systemLangs: [],
		allowSystemHunspell: true,
		preferSystemHunspell: true,
	};
};
const applyResolvedToSession = async (
	session: Session,
	state: SpellcheckState,
	resolved: ResolvedEngine,
	shouldContinue: () => boolean,
): Promise<ResolvedEngine> => {
	if (resolved.mode === 'off') {
		if (!shouldContinue()) return resolved;
		await unloadAll(session);
		if (!shouldContinue()) return resolved;
		if (useChromiumSpellchecker) {
			session.setSpellCheckerEnabled(false);
		}
		return resolved;
	}
	if (resolved.mode === 'system') {
		if (!shouldContinue()) return resolved;
		await unloadAll(session);
		if (!shouldContinue()) return resolved;
		applySystemSpellcheck(session, resolved.systemLangs);
		return resolved;
	}
	if (!shouldContinue()) return resolved;
	if (useChromiumSpellchecker) {
		session.setSpellCheckerEnabled(true);
	}
	if (useChromiumSpellchecker && !isMac) {
		try {
			session.setSpellCheckerLanguages([]);
		} catch (error) {
			log.warn('[Spellcheck] clearing setSpellCheckerLanguages failed', error);
		}
	}
	const loadedCount = await syncLoadedHunspell(
		session,
		resolved.hunspellLangs,
		shouldContinue,
		resolved.allowSystemHunspell === true,
		resolved.preferSystemHunspell === true,
	);
	if (!shouldContinue()) return resolved;
	if (loadedCount === 0) {
		if (!useChromiumSpellchecker) {
			log.warn('[Spellcheck] No Hunspell dictionaries loaded; Chromium spellcheck fallback is disabled on Linux');
			return {mode: 'off', hunspellLangs: [], systemLangs: []};
		}
		const fallback = resolveSystemFallback(state, session);
		log.warn('[Spellcheck] No Hunspell dictionaries loaded; falling back to the system spellchecker');
		await unloadAll(session);
		if (!shouldContinue()) return fallback;
		applySystemSpellcheck(session, fallback.systemLangs);
		return fallback;
	}
	applyPersonalDictionary(session, state.personalDictionary);
	return resolved;
};
const cancelDictionaryRetry = (session: Session): void => {
	const timer = sessionRetryTimer.get(session);
	if (!timer) return;
	clearTimeout(timer);
	sessionRetryTimer.delete(session);
};
const scheduleDictionaryRetry = (webContents: WebContents): void => {
	const session = webContents.session;
	const attempt = sessionRetryAttempt.get(session) ?? 0;
	const delay = DICTIONARY_RETRY_DELAYS_MS[attempt];
	if (delay === undefined) return;
	sessionRetryAttempt.set(session, attempt + 1);
	cancelDictionaryRetry(session);
	const timer = setTimeout(() => {
		sessionRetryTimer.delete(session);
		if (webContents.isDestroyed()) return;
		const current = sessionState.get(session);
		if (!current?.enabled) return;
		log.info('[Spellcheck] Retrying dictionary load after an empty resolve', {attempt: attempt + 1});
		void applyStateToWebContents(webContents, current, {broadcastResolved: true});
	}, delay);
	timer.unref?.();
	sessionRetryTimer.set(session, timer);
};
const sendResolvedEngine = (webContents: WebContents, effective: ResolvedEngine): void => {
	try {
		webContents.send('spellcheck-engine-resolved', {
			mode: effective.mode,
			hunspellLangs: effective.hunspellLangs,
			systemLangs: effective.systemLangs,
		});
	} catch {}
};
const applyStateToWebContents = async (
	webContents: WebContents,
	state: SpellcheckState,
	opts: {
		broadcastState?: boolean;
		broadcastResolved?: boolean;
	} = {},
): Promise<ResolvedEngine> => {
	const session = webContents.session;
	const resolved = resolveEngine(state, session);
	let effective = resolved;
	const generation = beginSessionApply(session);
	try {
		effective = await applyResolvedToSession(session, state, resolved, () =>
			isSessionApplyCurrent(session, generation),
		);
	} catch (error) {
		log.error('[Spellcheck] apply failed', error);
	}
	if (!isSessionApplyCurrent(session, generation)) {
		const settled = sessionResolvedEngine.get(session);
		if (opts.broadcastResolved && settled) {
			sendResolvedEngine(webContents, settled);
		}
		return effective;
	}
	sessionResolvedEngine.set(session, effective);
	if (state.enabled && effective.mode === 'off') {
		scheduleDictionaryRetry(webContents);
	} else {
		sessionRetryAttempt.delete(session);
		cancelDictionaryRetry(session);
	}
	if (opts.broadcastState) {
		try {
			webContents.send('spellcheck-state-changed', state);
		} catch {}
	}
	if (opts.broadcastResolved) {
		sendResolvedEngine(webContents, effective);
	}
	return effective;
};
const applySystemSpellcheck = (session: Session, languages: Array<string>): void => {
	if (!useChromiumSpellchecker) return;
	session.setSpellCheckerEnabled(true);
	if (languages.length > 0 && !isMac) {
		try {
			session.setSpellCheckerLanguages(languages);
		} catch (error) {
			log.warn('[Spellcheck] setSpellCheckerLanguages failed in system mode', error);
		}
	}
};
const listAvailableSpellcheckLanguages = (session: Session): Array<string> => {
	if (!isLinux) {
		return session.availableSpellCheckerLanguages ?? [];
	}
	const state = sessionState.get(session) ?? defaultState;
	const launchMode = getSpellcheckLaunchMode(process.argv);
	const effectiveEngine = launchMode === 'default' || launchMode === 'off' ? state.engine : launchMode;
	const includeSystem = effectiveEngine !== 'hunspell';
	const seen = new Set<string>();
	const languages: Array<string> = [];
	const add = (tag: string): void => {
		const key = norm(tag);
		if (seen.has(key)) return;
		seen.add(key);
		languages.push(tag);
	};
	if (includeSystem) {
		const native = loadSystemHunspell();
		for (const entry of native?.discoverSystemDictionaries?.() ?? []) {
			add(entry.tag);
		}
	}
	for (const dict of BUNDLED_DICTIONARIES) {
		add(dict.tag);
	}
	return languages;
};
const unloadAll = async (session: Session): Promise<void> => {
	const loaded = sessionLoaded.get(session);
	if (!loaded || loaded.size === 0) return;
	let factory: HunspellFactory | null = null;
	for (const entry of loaded.values()) {
		if (!isNativeLoaded(entry)) {
			factory ??= await ensureFactory().catch(() => null);
		}
		disposeLoaded(entry, factory);
	}
	loaded.clear();
	sessionPersonalApplied.delete(session);
};
const syncLoadedHunspell = async (
	session: Session,
	wanted: Array<string>,
	shouldContinue: () => boolean,
	allowSystemFallback: boolean,
	preferSystem: boolean,
): Promise<number> => {
	if (wanted.length === 0) {
		if (!shouldContinue()) return sessionLoaded.get(session)?.size ?? 0;
		await unloadAll(session);
		return 0;
	}
	const map = sessionLoaded.get(session) ?? new Map<string, LoadedHunspell>();
	sessionLoaded.set(session, map);
	const wantedKeys = new Set(wanted.map(norm));
	const applied = sessionPersonalApplied.get(session);
	let factory: HunspellFactory | null = null;
	for (const [key, entry] of map.entries()) {
		if (!wantedKeys.has(norm(entry.tag))) {
			if (!isNativeLoaded(entry)) {
				factory ??= await ensureFactory().catch(() => null);
			}
			disposeLoaded(entry, factory);
			map.delete(key);
		}
	}
	for (const tag of wanted) {
		const key = norm(tag);
		if (map.has(key)) continue;
		const entry = await loadHunspellForTag(tag, shouldContinue, allowSystemFallback, preferSystem);
		if (!shouldContinue()) {
			if (entry) {
				if (!isNativeLoaded(entry)) {
					factory ??= await ensureFactory().catch(() => null);
				}
				disposeLoaded(entry, factory);
			}
			break;
		}
		if (entry) {
			if (applied) {
				for (const word of applied) {
					try {
						entry.hunspell.addWord(word);
					} catch {}
				}
			}
			map.set(key, entry);
		}
	}
	return map.size;
};
const applyPersonalDictionary = (session: Session, words: Array<string>): void => {
	const loaded = sessionLoaded.get(session);
	if (!loaded || loaded.size === 0) return;
	const previous = sessionPersonalApplied.get(session) ?? new Set<string>();
	const desired = new Set(words);
	for (const w of previous) {
		if (!desired.has(w)) {
			for (const entry of loaded.values()) {
				try {
					entry.hunspell.removeWord(w);
				} catch {}
			}
		}
	}
	for (const w of desired) {
		if (!previous.has(w)) {
			for (const entry of loaded.values()) {
				try {
					entry.hunspell.addWord(w);
				} catch {}
			}
		}
	}
	sessionPersonalApplied.set(session, desired);
};
const checkWords = (session: Session, words: Array<string>): Array<string> => {
	const loaded = sessionLoaded.get(session);
	if (!loaded || loaded.size === 0) return [];
	const result: Array<string> = [];
	const instances = [...loaded.values()];
	for (const word of words) {
		if (typeof word !== 'string' || word.length === 0) continue;
		let ok = false;
		for (const entry of instances) {
			try {
				if (entry.hunspell.spell(word)) {
					ok = true;
					break;
				}
			} catch {}
		}
		if (!ok) result.push(word);
	}
	return result;
};
const suggestWord = (session: Session, word: string): Array<string> => {
	const loaded = sessionLoaded.get(session);
	if (!loaded || loaded.size === 0) return [];
	const out: Array<string> = [];
	const seen = new Set<string>();
	for (const entry of loaded.values()) {
		try {
			for (const s of entry.hunspell.suggest(word)) {
				if (!seen.has(s)) {
					seen.add(s);
					out.push(s);
				}
			}
		} catch {}
		if (out.length >= 12) break;
	}
	return out.slice(0, 12);
};
const ensureSharedIpc = () => {
	if (ipcRegistered) return;
	ipcRegistered = true;
	ipcMain.on(
		'spellcheck-context-target',
		(
			event,
			payload: {
				isTextarea?: boolean;
			},
		) => {
			contextSourceByWebContents.set(event.sender, {
				isTextarea: Boolean(payload?.isTextarea),
				ts: Date.now(),
			});
		},
	);
	ipcMain.on(
		'spellcheck:update-autodetect-text',
		(
			event,
			payload: {
				contextId?: string;
				text?: string;
			},
		) => {
			const session = event.sender.session;
			const state = sessionState.get(session) ?? {...defaultState};
			if (!state.enabled || !state.autoDetect) return;
			const text = typeof payload?.text === 'string' ? payload.text : '';
			const contextKey = getAutodetectContextKey(payload?.contextId);
			const nextLanguage = detectLanguageFromContextText(event.sender, contextKey, text);
			const currentLanguage = sessionAutoDetectedLanguage.get(session) ?? null;
			if (currentLanguage === nextLanguage) return;
			sessionAutoDetectedLanguage.set(session, nextLanguage);
			void applyStateToWebContents(event.sender, state, {broadcastResolved: true});
		},
	);
	ipcMain.handle('spellcheck:get-bundled-dictionaries', (): Array<SpellcheckBundledDictionary> => listBundled());
	ipcMain.handle('spellcheck:check-words', (event, words: Array<string>): Array<string> => {
		if (!Array.isArray(words)) return [];
		return checkWords(event.sender.session, words);
	});
	ipcMain.handle('spellcheck:suggest', (event, word: string): Array<string> => {
		if (typeof word !== 'string' || word.length === 0) return [];
		return suggestWord(event.sender.session, word);
	});
};
const shouldHandleContextMenu = (webContents: WebContents, params: Electron.ContextMenuParams): boolean => {
	if (!params['isEditable']) return false;
	const inputFieldType = (
		params as {
			inputFieldType?: string;
		}
	).inputFieldType;
	const isPassword =
		(
			params as {
				isPassword?: boolean;
			}
		).isPassword === true ||
		inputFieldType === 'password' ||
		(
			params as {
				formControlType?: string;
			}
		).formControlType === 'password';
	if (isPassword) return false;
	const target = contextSourceByWebContents.get(webContents);
	const targetRecent = target && Date.now() - target.ts < 5000;
	const isTextLike = inputFieldType === 'plainText' || inputFieldType === 'textarea' || inputFieldType === undefined;
	return Boolean((targetRecent && target.isTextarea) || isTextLike);
};
export const registerSpellcheck = (webContents: WebContents): void => {
	ensureSharedIpc();
	if (!launchModeLogged) {
		launchModeLogged = true;
		const launchMode = getSpellcheckLaunchMode(process.argv);
		if (launchMode !== 'default') {
			log.warn('[Spellcheck] Launch diagnostic override active', {mode: launchMode});
		}
	}
	const session = webContents.session;
	let state: SpellcheckState = applyLaunchSpellcheckMode(sessionState.get(session) ?? {...defaultState});
	sessionState.set(session, state);
	const applyAndBroadcast = async (broadcast: boolean) => {
		await applyStateToWebContents(webContents, state, {
			broadcastState: broadcast,
			broadcastResolved: broadcast,
		});
	};
	const setState = async (
		patch: RendererSpellcheckPatch | SpellcheckState,
		opts?: {
			broadcast?: boolean;
		},
	): Promise<SpellcheckState> => {
		state = applyLaunchSpellcheckMode(normalizeState(state, patch));
		sessionState.set(session, state);
		await applyAndBroadcast(opts?.broadcast !== false);
		return state;
	};
	void applyAndBroadcast(false);
	webContents.on('did-finish-load', () => {
		void applyStateToWebContents(webContents, sessionState.get(session) ?? state, {broadcastResolved: true});
	});
	if (!ipcMain.eventNames().includes('spellcheck-get-state')) {
		ipcMain.handle('spellcheck-get-state', (event) => {
			const targetSession = event.sender.session;
			const next = applyLaunchSpellcheckMode(sessionState.get(targetSession) ?? {...defaultState});
			sessionState.set(targetSession, next);
			return next;
		});
		ipcMain.handle('spellcheck-set-state', async (event, patch: RendererSpellcheckPatch) => {
			const targetSession = event.sender.session;
			const current = sessionState.get(targetSession) ?? {...defaultState};
			const next = applyLaunchSpellcheckMode(normalizeState(current, patch));
			sessionState.set(targetSession, next);
			await applyStateToWebContents(event.sender, next, {broadcastState: true, broadcastResolved: true});
			return next;
		});
		ipcMain.handle('spellcheck-get-available-languages', (event) =>
			listAvailableSpellcheckLanguages(event.sender.session),
		);
		ipcMain.handle('spellcheck-replace-misspelling', (event, replacement: string) => {
			event.sender.replaceMisspelling(replacement);
		});
		ipcMain.handle('spellcheck-add-word-to-dictionary', async (event, word: string) => {
			if (typeof word !== 'string' || word.length === 0) return;
			const targetSession = event.sender.session;
			const current = sessionState.get(targetSession) ?? {...defaultState};
			const next = applyLaunchSpellcheckMode(
				normalizeState(current, {
					personalDictionary: [...current.personalDictionary, word],
				}),
			);
			sessionState.set(targetSession, next);
			if (useChromiumSpellchecker) {
				try {
					targetSession.addWordToSpellCheckerDictionary(word);
				} catch {}
			}
			await applyStateToWebContents(event.sender, next, {broadcastState: true});
		});
	}
	webContents.on('context-menu', (event, params) => {
		if (!shouldHandleContextMenu(webContents, params)) return;
		event.preventDefault();
		const cur = sessionState.get(session) ?? {...defaultState};
		const resolved = sessionResolvedEngine.get(session) ?? resolveEngine(cur, session);
		const misspelledWord = params['misspelledWord'] as string | undefined;
		let suggestions: Array<string> = [];
		if (cur.enabled && misspelledWord) {
			if (resolved.mode === 'hunspell') {
				suggestions = suggestWord(session, misspelledWord);
			} else {
				suggestions = (params['dictionarySuggestions'] as Array<string> | undefined) ?? [];
			}
		}
		webContents.send('textarea-context-menu', {
			misspelledWord: cur.enabled ? misspelledWord : undefined,
			suggestions: cur.enabled && misspelledWord ? suggestions : [],
			editFlags: {
				canUndo: params['editFlags']['canUndo'],
				canRedo: params['editFlags']['canRedo'],
				canCut: params['editFlags']['canCut'],
				canCopy: params['editFlags']['canCopy'],
				canPaste: params['editFlags']['canPaste'],
				canSelectAll: params['editFlags']['canSelectAll'],
			},
			x: params['x'],
			y: params['y'],
		});
	});
	(
		webContents as WebContents & {
			__fluxerSpellcheckSetState?: typeof setState;
		}
	).__fluxerSpellcheckSetState = setState;
};
