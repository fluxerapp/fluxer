// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const REPO_ROOT = path.resolve(__dirname, '../../..');
const GENERATED_HEADER = '// SPDX-License-Identifier: AGPL-3.0-or-later\n\n';
const EMAIL_MESSAGES_MODULE = path.join(REPO_ROOT, 'fluxer_api/pkgs/email/src/email_i18n/EmailI18nMessages.ts');
const EMAIL_MESSAGES_EXPORT = 'EMAIL_I18N_MESSAGES';
const EMAIL_TYPES_OUTPUT = path.join(REPO_ROOT, 'fluxer_api/pkgs/email/src/email_i18n/EmailI18nTypes.generated.ts');

async function extractKeysFromStaticMessages(filePath: string, exportName: string): Promise<Array<string>> {
	const module = await import(pathToFileURL(filePath).href);
	const messages = module[exportName];
	if (!messages || typeof messages !== 'object' || Array.isArray(messages)) {
		throw new Error(`static messages export not found: ${exportName}`);
	}
	return Object.keys(messages).sort();
}

function generateEmailI18nTypes(keys: Array<string>): string {
	const unionType = keys.map((key) => `\t| '${key}'`).join('\n');
	return `export type EmailTemplateKey =
${unionType};

export interface EmailTemplate {
	subject: string;
	body: string;
}
`;
}

async function main(): Promise<void> {
	console.log('generating i18n types...\n');
	const keys = await extractKeysFromStaticMessages(EMAIL_MESSAGES_MODULE, EMAIL_MESSAGES_EXPORT);
	fs.mkdirSync(path.dirname(EMAIL_TYPES_OUTPUT), {recursive: true});
	fs.writeFileSync(EMAIL_TYPES_OUTPUT, `${GENERATED_HEADER}${generateEmailI18nTypes(keys)}`, 'utf8');
	console.log(`generated types for @pkgs/email (${keys.length} keys) -> ${EMAIL_TYPES_OUTPUT}`);
	console.log('\nall i18n types generated successfully!');
}

await main();
