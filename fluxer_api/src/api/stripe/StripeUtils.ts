// SPDX-License-Identifier: AGPL-3.0-or-later

export function extractId(
	value:
		| string
		| {
				id: string;
		  }
		| null
		| undefined,
): string | null {
	if (!value) return null;
	if (typeof value === 'string') return value || null;
	return value.id || null;
}

export function addMonthsClamp(date: Date, months: number): Date {
	const d = new Date(date);
	const originalDay = d.getUTCDate();
	const targetMonth = d.getUTCMonth() + months;
	d.setUTCMonth(targetMonth);
	if (d.getUTCDate() < originalDay) {
		d.setUTCDate(0);
	}
	return d;
}

export function isMissingOrCancelledSubscriptionError(error: unknown): boolean {
	if (!(error instanceof Error)) {
		return false;
	}
	const normalizedMessage = error.message.toLowerCase();
	return (
		normalizedMessage.includes('no such subscription') ||
		normalizedMessage.includes('cannot cancel a canceled subscription') ||
		normalizedMessage.includes('cannot cancel a cancelled subscription') ||
		normalizedMessage.includes('subscription is canceled') ||
		normalizedMessage.includes('subscription is cancelled')
	);
}
