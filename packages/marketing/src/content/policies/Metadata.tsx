/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

export interface PolicyMetadata {
	slug: string;
	title: string;
	description: string;
	category: string | null;
	lastUpdated: string;
}

export const POLICY_METADATA: ReadonlyArray<PolicyMetadata> = [
	{
		slug: 'terms',
		title: 'Terms of Service',
		description: 'The legal agreement between you and Fluxer that governs your use of our platform and services.',
		category: 'Legal',
		lastUpdated: '2026-02-13',
	},
	{
		slug: 'privacy',
		title: 'Privacy Policy',
		description:
			'How we collect, use, and protect your personal information when you use Fluxer. Your privacy matters to us.',
		category: 'Legal',
		lastUpdated: '2026-02-18',
	},
	{
		slug: 'guidelines',
		title: 'Community Guidelines',
		description:
			'The rules and expectations for participating in the Fluxer community. Help us keep Fluxer safe and welcoming.',
		category: 'Community',
		lastUpdated: '2026-02-21',
	},
	{
		slug: 'security',
		title: 'Security Bug Bounty',
		description:
			'Information about responsible disclosure and our security bug bounty program. Help us keep Fluxer secure.',
		category: 'Security',
		lastUpdated: '2026-02-13',
	},
	{
		slug: 'company-information',
		title: 'Company Information',
		description: 'Legal information about Fluxer Platform AB, including our registered address and contact details.',
		category: 'Legal',
		lastUpdated: '2026-02-13',
	},
];

export function getPolicyMetadata(slug: string): PolicyMetadata | null {
	return POLICY_METADATA.find((policy) => policy.slug === slug) ?? null;
}
