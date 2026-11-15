// SPDX-License-Identifier: AGPL-3.0-or-later

export function parseInviteCodeList(value: string | null | undefined): Array<string> {
	return [
		...new Set(
			(value ?? '')
				.split(/[\s,]+/)
				.map((code) => code.trim())
				.filter(Boolean),
		),
	];
}
