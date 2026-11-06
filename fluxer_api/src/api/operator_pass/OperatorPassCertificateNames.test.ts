// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isAcceptableOperatorPassCertificateName,
	isOperatorPassReservedName,
} from '@app/api/operator_pass/OperatorPassCertificateNames';
import {expect, test} from 'vitest';

test.each([
	'Ada Lovelace',
	'Əli Məmmədov',
	'Kwame Ɔsɛi',
	'Łukasz Ørsted',
	'A+B Consulting',
	'Ελένη Smith',
	'李小龍',
	'宮崎 駿',
	'김연아',
	'محمد بن سلمان',
	'نسرین ستوده‌نیا',
	'דוד בן־גוריון',
	'श्रीनिवास रामानुजन',
	'சுப்பிரமணிய பாரதி',
	'ภูมิพลอดุลยเดช',
	'ኃይለ ሥላሴ',
	'ᠴᠢᠩᠭᠢᠰ ᠬᠠᠭᠠᠨ',
	'Anna 李',
	'𠮷野家 太郎',
])('accepts the certificate name %s', (name) => {
	expect(isAcceptableOperatorPassCertificateName(name)).toBe(true);
});

test.each([
	'ꜰʟᴜxᴇʀ',
	'ꜱtaff',
	'plutonıum',
	'płutonium',
	'ƒluxer',
	'Fluxɛr Team',
	'fӏuxer',
	'F|uxer',
	'Ｆｌｕｘｅｒ',
	'Fluхer',
	'Ꮪtaff',
	'Ⅾiscord',
	'ADMIN',
	'Ada 🚀',
	'Ada‮ecalevol',
	'‌abc',
	`A${'́'.repeat(12)}`,
	'寿'.repeat(49),
])('turns down the certificate name %s', (name) => {
	expect(isAcceptableOperatorPassCertificateName(name)).toBe(false);
});

test.each([
	['ꜰʟᴜxᴇʀ', true],
	['Fluхer', true],
	['Ѕtripe', true],
	['Steam Team', true],
	['ΜΕΤΑ Inc', true],
	['ΜΕΤΑ TWITCH', true],
	['Lisa Inc', false],
	['Ilya Inc', false],
	['Ivan Иванов', false],
	['Mighty Mike', false],
])('checks the wall name %s', (name, reserved) => {
	expect(isOperatorPassReservedName(name)).toBe(reserved);
});
