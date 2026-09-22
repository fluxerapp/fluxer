// SPDX-License-Identifier: AGPL-3.0-or-later

import reactGoogleTranslate from 'eslint-plugin-react-google-translate';
import tseslint from 'typescript-eslint';

export default [
	{
		ignores: [
			'**/node_modules/**',
			'**/dist/**',
			'**/build/**',
			'**/coverage/**',
			'**/*.generated.*',
			'fluxer_app/src/features/i18n/locales/*/messages.mjs',
		],
	},
	{
		files: ['fluxer_app/src/**/*.tsx'],
		linterOptions: {
			reportUnusedDisableDirectives: 'error',
		},
		languageOptions: {
			parser: tseslint.parser,
			parserOptions: {
				project: './fluxer_app/tsconfig.json',
				tsconfigRootDir: import.meta.dirname,
			},
		},
		plugins: {'react-google-translate': reactGoogleTranslate},
		rules: {
			'react-google-translate/no-conditional-text-nodes-with-siblings': [
				'error',
				{ignoreParents: ['Trans', 'Plural', 'Select', 'SelectOrdinal']},
			],
			'react-google-translate/no-return-text-nodes': 'error',
		},
	},
];
