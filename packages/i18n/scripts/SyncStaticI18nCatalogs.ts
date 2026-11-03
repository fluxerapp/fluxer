import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';
import {parseArgs} from 'node:util';
import {STATIC_I18N_LOCALES, type StaticI18nLocale} from '@fluxer/i18n/src/runtime/StaticLocaleFiles';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const REPO_ROOT = path.resolve(__dirname, '../../..');

type FlatCatalog = Record<string, string>;

interface EmailTemplate {
	subject: string;
	intro: string;
	[field: string]: string;
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
}

const CATALOGS: Array<StaticCatalogConfig> = [
	{
		name: 'errors',
		kind: 'flat',
		sourceModulePath: path.join(REPO_ROOT, 'packages/errors/src/i18n/ErrorI18nMessages.ts'),
		sourceExportName: 'ERROR_I18N_MESSAGES',
		weblateDir: path.join(REPO_ROOT, 'packages/errors/src/i18n/weblate'),
	},
	{
		name: 'email',
		kind: 'email',
		sourceModulePath: path.join(REPO_ROOT, 'fluxer_api/pkgs/email/src/email_i18n/EmailI18nMessages.ts'),
		sourceExportName: 'EMAIL_I18N_MESSAGES',
		weblateDir: path.join(REPO_ROOT, 'fluxer_api/pkgs/email/src/email_i18n/weblate'),
	},
	{
		name: 'content',
		kind: 'flat',
		sourceModulePath: path.join(REPO_ROOT, 'fluxer_api/src/api/content_i18n/ContentI18nMessages.ts'),
		sourceExportName: 'CONTENT_I18N_MESSAGES',
		weblateDir: path.join(REPO_ROOT, 'fluxer_api/src/api/content_i18n/weblate'),
	},
];

function ensureDir(dir: string): void {
	fs.mkdirSync(dir, {recursive: true});
}

function jsonPath(config: StaticCatalogConfig, locale?: StaticI18nLocale): string {
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

function sortCatalogForWeblate(config: StaticCatalogConfig, catalog: Catalog): Catalog {
	if (config.kind !== 'email') {
		return sortKeys(catalog as FlatCatalog);
	}
	const sorted: EmailCatalog = {};
	for (const [key, template] of Object.entries(sortKeys(catalog as EmailCatalog))) {
		sorted[key] = sortKeys(template) as EmailTemplate;
	}
	return sorted;
}

function writeJson(filePath: string, value: unknown): void {
	ensureDir(path.dirname(filePath));
	fs.writeFileSync(filePath, `${JSON.stringify(value, null, '\t')}\n`, 'utf8');
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
		const existingValue = existingRecord[key];
		const existingTemplate = isRecord(existingValue) ? existingValue : {};
		const template: Record<string, string> = {};
		for (const field of Object.keys(sourceTemplate)) {
			const value = existingTemplate[field];
			template[field] = typeof value === 'string' ? value : sourceTemplate[field];
		}
		next[key] = template as EmailTemplate;
	}
	return next;
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
		if (!isRecord(value) || typeof value.subject !== 'string' || typeof value.intro !== 'string') {
			throw new Error(`${label}.${key} must contain string subject and intro fields`);
		}
		for (const [field, text] of Object.entries(value)) {
			if (!/^[a-z][a-z0-9_]*$/.test(field) || typeof text !== 'string') {
				throw new Error(`${label}.${key}.${field} must be a snake_case string field`);
			}
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
	writeJson(jsonPath(config), sortKeys<Catalog[string]>(source));

	for (const locale of STATIC_I18N_LOCALES) {
		const next = normalizeCatalog(config, source, readJson<unknown>(jsonPath(config, locale)));
		writeJson(jsonPath(config, locale), sortCatalogForWeblate(config, next));
	}
	console.log(`extracted static ${config.name} catalogs`);
}

interface SyncOptions {
	catalogs: Array<StaticCatalogConfig>;
}

function parseSyncOptions(argv: Array<string>): SyncOptions {
	const {values} = parseArgs({
		args: argv,
		options: {
			catalog: {type: 'string', multiple: true, default: []},
		},
		strict: true,
		allowPositionals: false,
	});
	const known = CATALOGS.map((config) => config.name);
	const unknown = values.catalog.filter((name) => !known.includes(name));
	if (unknown.length > 0) {
		throw new Error(`unknown catalog ${unknown.join(', ')}, expected one of ${known.join(', ')}`);
	}
	return {
		catalogs:
			values.catalog.length === 0 ? CATALOGS : CATALOGS.filter((config) => values.catalog.includes(config.name)),
	};
}

async function main(): Promise<void> {
	const options = parseSyncOptions(process.argv.slice(2));
	for (const config of options.catalogs) {
		await extractCatalog(config);
	}
}

await main();
