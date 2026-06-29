// SPDX-License-Identifier: AGPL-3.0-or-later

export function isVisionaryDiscriminator0000Blocked(options: {
	showPremium: boolean;
	isVisionary: boolean;
	discriminator: string;
}): boolean {
	const {showPremium, isVisionary, discriminator} = options;
	return showPremium && !isVisionary && discriminator === '0000';
}
