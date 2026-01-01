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

import type {ReactNode} from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Link as RouterLink} from '~/lib/router';

interface AuthRouterLinkProps {
	ringOffset?: number;
	children?: ReactNode;
	className?: string;
	to: string;
	search?: Record<string, string | undefined>;
}

export function AuthRouterLink({ringOffset = -2, children, className, to, search}: AuthRouterLinkProps) {
	return (
		<FocusRing offset={ringOffset}>
			<RouterLink tabIndex={0} className={className} to={to} search={search}>
				{children}
			</RouterLink>
		</FocusRing>
	);
}
