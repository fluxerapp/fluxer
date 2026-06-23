// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const REPO_ROOT = path.resolve(__dirname, '../../..');
const GENERATED_HEADER = '// SPDX-License-Identifier: AGPL-3.0-or-later\n\n';

const STATIC_I18N_LOCALES = [
	'ar',
	'bg',
	'cs',
	'da',
	'de',
	'el',
	'en-GB',
	'es-419',
	'es-ES',
	'fi',
	'fr',
	'he',
	'hi',
	'hr',
	'hu',
	'id',
	'it',
	'ja',
	'ko',
	'lt',
	'nl',
	'no',
	'pl',
	'pt-BR',
	'ro',
	'ru',
	'sv-SE',
	'th',
	'tr',
	'uk',
	'vi',
	'zh-CN',
	'zh-TW',
] as const;

type StaticLocale = (typeof STATIC_I18N_LOCALES)[number];
type FlatCatalog = Record<string, string>;

interface EmailTemplate {
	subject: string;
	body: string;
}

type EmailCatalog = Record<string, EmailTemplate>;
type CatalogKind = 'flat' | 'email';
type Catalog = FlatCatalog | EmailCatalog;

interface StaticCatalogConfig {
	name: string;
	kind: CatalogKind;
	sourceModulePath: string;
	sourceExportName: string;
	weblateDir: string;
	localeTsDir: string;
	defineImportPath: string;
	defineFunctionName: string;
	exportPrefix: string;
}

const CATALOGS: Array<StaticCatalogConfig> = [
	{
		name: 'errors',
		kind: 'flat',
		sourceModulePath: path.join(REPO_ROOT, 'packages/errors/src/i18n/ErrorI18nMessages.ts'),
		sourceExportName: 'ERROR_I18N_MESSAGES',
		weblateDir: path.join(REPO_ROOT, 'packages/errors/src/i18n/weblate'),
		localeTsDir: path.join(REPO_ROOT, 'packages/errors/src/i18n/locales'),
		defineImportPath: '../ErrorI18nMessages',
		defineFunctionName: 'defineErrorI18nLocaleMessages',
		exportPrefix: 'ERROR_I18N',
	},
	{
		name: 'email',
		kind: 'email',
		sourceModulePath: path.join(REPO_ROOT, 'fluxer_api/pkgs/email/src/email_i18n/EmailI18nMessages.ts'),
		sourceExportName: 'EMAIL_I18N_MESSAGES',
		weblateDir: path.join(REPO_ROOT, 'fluxer_api/pkgs/email/src/email_i18n/weblate'),
		localeTsDir: path.join(REPO_ROOT, 'fluxer_api/pkgs/email/src/email_i18n/locales'),
		defineImportPath: '../EmailI18nMessages',
		defineFunctionName: 'defineEmailI18nLocaleMessages',
		exportPrefix: 'EMAIL_I18N',
	},
	{
		name: 'content',
		kind: 'flat',
		sourceModulePath: path.join(REPO_ROOT, 'fluxer_api/src/api/content_i18n/ContentI18nMessages.ts'),
		sourceExportName: 'CONTENT_I18N_MESSAGES',
		weblateDir: path.join(REPO_ROOT, 'fluxer_api/src/api/content_i18n/weblate'),
		localeTsDir: path.join(REPO_ROOT, 'fluxer_api/src/api/content_i18n/locales'),
		defineImportPath: '../ContentI18nMessages',
		defineFunctionName: 'defineContentI18nLocaleMessages',
		exportPrefix: 'CONTENT_I18N',
	},
];

function ensureDir(dir: string): void {
	fs.mkdirSync(dir, {recursive: true});
}

function jsonPath(config: StaticCatalogConfig, locale?: StaticLocale): string {
	if (!locale) {
		return path.join(config.weblateDir, 'messages.json');
	}
	return path.join(config.weblateDir, 'locales', `${locale}.json`);
}

async function importExport<T>(modulePath: string, exportName: string): Promise<T> {
	const module = await import(pathToFileURL(modulePath).href);
	const value = module[exportName];
	if (!value || typeof value !== 'object' || Array.isArray(value)) {
		throw new Error(`missing object export ${exportName} in ${modulePath}`);
	}
	return value as T;
}

async function importDefault<T>(modulePath: string): Promise<T | null> {
	if (!fs.existsSync(modulePath)) {
		return null;
	}
	const module = await import(pathToFileURL(modulePath).href);
	if (!module.default || typeof module.default !== 'object' || Array.isArray(module.default)) {
		return null;
	}
	return module.default as T;
}

function readJson<T>(filePath: string): T | null {
	if (!fs.existsSync(filePath)) {
		return null;
	}
	return JSON.parse(fs.readFileSync(filePath, 'utf8')) as T;
}

function sortKeys<T>(record: Record<string, T>): Record<string, T> {
	const sorted: Record<string, T> = {};
	for (const key of Object.keys(record).sort()) {
		sorted[key] = record[key];
	}
	return sorted;
}

function writeJson(filePath: string, value: unknown): void {
	ensureDir(path.dirname(filePath));
	fs.writeFileSync(filePath, `${JSON.stringify(value, null, '\t')}\n`, 'utf8');
}

function localeTsPath(config: StaticCatalogConfig, locale: StaticLocale): string {
	return path.join(config.localeTsDir, `${locale}.ts`);
}

function exportName(config: StaticCatalogConfig, locale: StaticLocale): string {
	const normalizedLocale = locale.replace(/[^a-zA-Z0-9]+/g, '_').toUpperCase();
	return `${config.exportPrefix}_${normalizedLocale}_MESSAGES`;
}

function normalizeFlatCatalog(source: FlatCatalog, existing: unknown): FlatCatalog {
	const existingRecord = isRecord(existing) ? existing : {};
	const next: FlatCatalog = {};
	for (const key of Object.keys(source).sort()) {
		const value = existingRecord[key];
		next[key] = typeof value === 'string' ? value : source[key];
	}
	return next;
}

function normalizeEmailCatalog(source: EmailCatalog, existing: unknown): EmailCatalog {
	const existingRecord = isRecord(existing) ? existing : {};
	const next: EmailCatalog = {};
	for (const key of Object.keys(source).sort()) {
		const sourceTemplate = source[key];
		const existingTemplate = existingRecord[key];
		next[key] = {
			subject:
				isRecord(existingTemplate) && typeof existingTemplate.subject === 'string'
					? existingTemplate.subject
					: sourceTemplate.subject,
			body:
				isRecord(existingTemplate) && typeof existingTemplate.body === 'string'
					? existingTemplate.body
					: sourceTemplate.body,
		};
	}
	return next;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return Boolean(value) && typeof value === 'object' && !Array.isArray(value);
}

function validateCatalog(kind: CatalogKind, catalog: unknown, label: string): Catalog {
	if (!isRecord(catalog)) {
		throw new Error(`${label} must be an object`);
	}
	if (kind === 'flat') {
		for (const [key, value] of Object.entries(catalog)) {
			if (typeof value !== 'string') {
				throw new Error(`${label}.${key} must be a string`);
			}
		}
		return catalog as FlatCatalog;
	}
	for (const [key, value] of Object.entries(catalog)) {
		if (!isRecord(value) || typeof value.subject !== 'string' || typeof value.body !== 'string') {
			throw new Error(`${label}.${key} must contain string subject and body fields`);
		}
	}
	return catalog as EmailCatalog;
}

function normalizeCatalog(config: StaticCatalogConfig, source: Catalog, existing: unknown): Catalog {
	return config.kind === 'email'
		? normalizeEmailCatalog(source as EmailCatalog, existing)
		: normalizeFlatCatalog(source as FlatCatalog, existing);
}

async function extractCatalog(config: StaticCatalogConfig): Promise<void> {
	const source = validateCatalog(
		config.kind,
		await importExport(config.sourceModulePath, config.sourceExportName),
		`${config.name} source catalog`,
	);
	writeJson(jsonPath(config), sortKeys(source));

	for (const locale of STATIC_I18N_LOCALES) {
		const existingJson = readJson<unknown>(jsonPath(config, locale));
		const existingTs = existingJson === null ? await importDefault<Catalog>(localeTsPath(config, locale)) : null;
		const next = normalizeCatalog(config, source, existingJson ?? existingTs);
		writeJson(jsonPath(config, locale), sortKeys(next));
	}
	console.log(`extracted static ${config.name} catalogs`);
}

function renderTs(config: StaticCatalogConfig, locale: StaticLocale, catalog: Catalog): string {
	const name = exportName(config, locale);
	return `${GENERATED_HEADER}import {${config.defineFunctionName}} from '${config.defineImportPath}';

const ${name} = ${config.defineFunctionName}(${JSON.stringify(catalog, null, '\t')});

export default ${name};
`;
}

async function compileCatalog(config: StaticCatalogConfig): Promise<void> {
	const source = validateCatalog(
		config.kind,
		await importExport(config.sourceModulePath, config.sourceExportName),
		`${config.name} source catalog`,
	);
	for (const locale of STATIC_I18N_LOCALES) {
		const existing = readJson<unknown>(jsonPath(config, locale));
		const catalog = normalizeCatalog(config, source, existing);
		ensureDir(config.localeTsDir);
		fs.writeFileSync(localeTsPath(config, locale), renderTs(config, locale, catalog), 'utf8');
	}
	console.log(`compiled static ${config.name} locale modules`);
}

async function main(): Promise<void> {
	const args = new Set(process.argv.slice(2));
	const shouldExtract = args.has('--extract');
	const shouldCompile = args.has('--compile');
	if (!shouldExtract && !shouldCompile) {
		throw new Error('usage: SyncStaticI18nCatalogs.ts --extract [--compile]');
	}
	for (const config of CATALOGS) {
		if (shouldExtract) {
			await extractCatalog(config);
		}
		if (shouldCompile) {
			await compileCatalog(config);
		}
	}
}

await main();
