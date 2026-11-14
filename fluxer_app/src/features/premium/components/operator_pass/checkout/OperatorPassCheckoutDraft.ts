// SPDX-License-Identifier: AGPL-3.0-or-later

import {getStoredOperatorPassLabsGreeting} from '@app/features/premium/components/operator_pass/OperatorPassLabsGreetingField';
import {
	getOperatorPassCertificateNameProblem,
	normalizeOperatorPassCertificateName,
	type OperatorPassCertificateNameProblem,
	type OperatorPassWallNameProblem,
} from '@app/features/premium/utils/OperatorPassUtils';
import {truncateOperatorPassCertificateName} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {makeAutoObservable} from 'mobx';

export const CERTIFICATE_NAME_FIELD_ID = 'operator-pass-checkout-certificate-name';
export const WALL_NAME_FIELD_ID = 'operator-pass-checkout-wall-name';
export const DELIVERY_BOX_ID = 'operator-pass-checkout-delivery';
export const AGE_BOX_ID = 'operator-pass-checkout-age';
export const LABS_GREETING_FIELD_ID = 'operator-pass-checkout-labs-greeting';

export class OperatorPassCheckoutDraft {
	quantity = 1;
	certificateNames: Array<string>;
	sameCertificateName = true;
	certificateNameProblem: OperatorPassCertificateNameProblem | null = null;
	certificateNameProblemIndex: number | null = null;
	wallName: string;
	wallOptOut = false;
	wallNameProblem: OperatorPassWallNameProblem | null = null;
	labsGreeting = getStoredOperatorPassLabsGreeting();
	delivery = false;
	ageConfirmed = false;
	deliveryMissing = false;
	deliveryUnticked = false;
	ageMissing = false;
	revealWallField = false;
	isBusiness = false;
	cnpj = '';
	cnpjError: string | null = null;

	constructor(displayName: string) {
		this.wallName = displayName;
		this.certificateNames = [truncateOperatorPassCertificateName(normalizeOperatorPassCertificateName(displayName))];
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setQuantity(value: number): void {
		if (this.quantity > 1 !== value > 1) this.resetDelivery();
		this.quantity = value;
	}
	setCertificateName(index: number, value: string): void {
		const next = [...this.certificateNames];
		next[index] = value;
		this.certificateNames = next;
		if (this.certificateNameProblem && (this.certificateNameProblemIndex ?? 0) === index) {
			this.certificateNameProblem = getOperatorPassCertificateNameProblem(value);
		}
	}
	setSameCertificateName(value: boolean): void {
		this.sameCertificateName = value;
		if (!value) {
			const first = this.certificateNames[0] ?? '';
			this.certificateNames = Array.from(
				{length: Math.max(this.certificateNames.length, this.quantity)},
				(_, index) => this.certificateNames[index] || first,
			);
		}
		this.setCertificateNameProblem(null, null);
	}
	setCertificateNameProblem(problem: OperatorPassCertificateNameProblem | null, index: number | null): void {
		this.certificateNameProblem = problem;
		this.certificateNameProblemIndex = index;
	}
	setWallName(value: string): void {
		this.wallName = value;
	}
	setWallOptOut(value: boolean): void {
		this.wallOptOut = value;
	}
	setWallNameProblem(value: OperatorPassWallNameProblem | null): void {
		this.wallNameProblem = value;
	}
	setLabsGreeting(value: boolean): void {
		this.labsGreeting = value;
	}
	setDelivery(value: boolean): void {
		this.delivery = value;
		this.deliveryUnticked = false;
		if (value) this.deliveryMissing = false;
	}
	setAgeConfirmed(value: boolean): void {
		this.ageConfirmed = value;
		if (value) this.ageMissing = false;
	}
	setDeliveryMissing(value: boolean): void {
		this.deliveryMissing = value;
	}
	setAgeMissing(value: boolean): void {
		this.ageMissing = value;
	}
	setRevealWallField(value: boolean): void {
		this.revealWallField = value;
	}
	setIsBusiness(value: boolean): void {
		this.isBusiness = value;
	}
	setCnpj(value: string): void {
		this.cnpj = value;
		this.cnpjError = null;
	}
	setCnpjError(value: string | null): void {
		this.cnpjError = value;
	}

	get certificateNamesToSend(): Array<string> {
		const first = normalizeOperatorPassCertificateName(this.certificateNames[0] ?? '');
		if (this.quantity <= 1 || this.sameCertificateName) return [first];
		return Array.from({length: this.quantity}, (_, index) =>
			normalizeOperatorPassCertificateName(this.certificateNames[index] ?? ''),
		);
	}

	findCertificateNameProblem(): {problem: OperatorPassCertificateNameProblem; index: number} | null {
		const names = this.certificateNamesToSend;
		for (const [index, name] of names.entries()) {
			const problem = getOperatorPassCertificateNameProblem(name);
			if (problem) return {problem, index};
		}
		return null;
	}

	resetDelivery(): void {
		this.deliveryUnticked ||= this.delivery;
		this.delivery = false;
		this.deliveryMissing = false;
	}

	resetConsents(): void {
		this.delivery = false;
		this.ageConfirmed = false;
		this.deliveryMissing = false;
		this.deliveryUnticked = false;
		this.ageMissing = false;
	}
}
