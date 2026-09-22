// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DonorMagicLinkTokenRow} from '@app/api/database/types/DonationTypes';

export class DonorMagicLinkToken {
	readonly token: string;
	readonly donorEmail: string;
	readonly expiresAt: Date;
	readonly usedAt: Date | null;

	constructor(row: DonorMagicLinkTokenRow) {
		this.token = row.token_;
		this.donorEmail = row.donor_email;
		this.expiresAt = row.expires_at;
		this.usedAt = row.used_at ?? null;
	}

	toRow(): DonorMagicLinkTokenRow {
		return {
			token_: this.token,
			donor_email: this.donorEmail,
			expires_at: this.expiresAt,
			used_at: this.usedAt,
		};
	}

	isExpired(): boolean {
		return new Date() > this.expiresAt;
	}

	isUsed(): boolean {
		return this.usedAt !== null;
	}

	isValid(): boolean {
		return !this.isExpired() && !this.isUsed();
	}
}
