// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {canPrintOperatorPassCertificateName} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificateNameText';
import {
	combineOperatorPassCertificatePages,
	getOperatorPassCertificateNameNote,
	measureOperatorPassCertificateName,
	measureOperatorPassCertificateVerificationLine,
	renderOperatorPassCertificatePage,
} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificatePdf';
import {PDFDocument} from 'pdf-lib';
import {describe, expect, it} from 'vitest';

const PAGE = {
	number: 123,
	paidAt: new Date('2026-11-01T16:03:27Z'),
	earlyBird: true,
	termsVersion: '2026-11-01',
	issuedAt: new Date('2026-11-01T16:03:29Z'),
	copy: 'original' as const,
	source: 'purchase' as const,
	verificationUrl: 'https://fluxer.com/pass/c/Q2VydGlmaWNhdGVfTGluaw',
};

function sha256(bytes: Uint8Array): string {
	return createHash('sha256').update(bytes).digest('hex');
}

describe('Operator certificate PDF', () => {
	it.each([
		['Latin', 'Zoë Ørsted-Łukasiewicz'],
		['Vietnamese Latin', 'Nguyễn Thị Minh Khai'],
		['Greek', 'Ελένη Παπαδοπούλου'],
		['Cyrillic', 'Анна Смирнова'],
		['Chinese', '李小龍'],
		['Japanese', '宮崎 駿'],
		['Korean', '김연아'],
		['Arabic', 'محمد بن سلمان'],
		['Hebrew', 'דוד בן־גוריון'],
		['Devanagari', 'श्रीनिवास रामानुजन'],
		['Thai', 'ภูมิพลอดุลยเดช'],
		['Ethiopic', 'ኃይለ ሥላሴ'],
	])('renders a %s name the same way every time', async (_script, holderName) => {
		expect(await canPrintOperatorPassCertificateName(holderName)).toBe(true);
		const first = await renderOperatorPassCertificatePage({...PAGE, holderName});
		const second = await renderOperatorPassCertificatePage({...PAGE, holderName});
		expect(sha256(first)).toBe(sha256(second));
		expect((await PDFDocument.load(first)).getPageCount()).toBe(1);
	});

	it('says a name was updated only on a copy whose name changed', () => {
		const issuedAt = new Date('2026-12-03T09:00:00Z');
		expect(getOperatorPassCertificateNameNote(PAGE)).toBe('This name was chosen at checkout.');
		expect(getOperatorPassCertificateNameNote({issuedAt, copy: 'same_name', source: 'purchase'})).toBe(
			'This copy was issued on 3 December 2026.',
		);
		expect(getOperatorPassCertificateNameNote({issuedAt, copy: 'new_name', source: 'purchase'})).toBe(
			'This copy was issued on 3 December 2026 with an updated name.',
		);
	});

	it('shrinks the widest accepted name until it fits inside the frame', async () => {
		for (const holderName of ['\u01C4'.repeat(48), '寿'.repeat(48)]) {
			expect(await canPrintOperatorPassCertificateName(holderName)).toBe(true);
			const {size, width, maxWidth} = await measureOperatorPassCertificateName(holderName);
			expect(width).toBeLessThanOrEqual(maxWidth + 0.001);
			expect(size).toBeLessThan(16);
		}
		expect((await measureOperatorPassCertificateName('Ada Lovelace')).size).toBe(40);
	});

	it('keeps the link line inside the frame at a long marketing address', async () => {
		const {width, maxWidth} = await measureOperatorPassCertificateVerificationLine(
			`https://marketing.canary.fluxer.app/pass/c/${'W'.repeat(22)}`,
		);
		expect(width).toBeLessThanOrEqual(maxWidth);
	});

	it('turns down names it has no font for', async () => {
		expect(await canPrintOperatorPassCertificateName('Ada 🚀')).toBe(false);
		expect(await canPrintOperatorPassCertificateName('\u{10900}\u{10901}')).toBe(false);
	});

	it('puts one page per pass in the combined file', async () => {
		const pages = await Promise.all(
			['Ada', 'Grace', 'Linus'].map((holderName, index) =>
				renderOperatorPassCertificatePage({...PAGE, number: PAGE.number + index, holderName}),
			),
		);
		const combined = await combineOperatorPassCertificatePages(pages, PAGE.number, PAGE.issuedAt);
		expect((await PDFDocument.load(combined)).getPageCount()).toBe(3);
	});
});
