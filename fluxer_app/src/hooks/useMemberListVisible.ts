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

import React from 'react';
import MemberListStore from '~/stores/MemberListStore';

const MIN_WIDTH_FOR_MEMBERS = 1024;

export const useMemberListVisible = (): boolean => {
	const {isMembersOpen} = MemberListStore;
	const [canFit, setCanFit] = React.useState(() => window.innerWidth >= MIN_WIDTH_FOR_MEMBERS);

	React.useEffect(() => {
		const checkWidth = () => {
			setCanFit(window.innerWidth >= MIN_WIDTH_FOR_MEMBERS);
		};

		window.addEventListener('resize', checkWidth);
		return () => window.removeEventListener('resize', checkWidth);
	}, []);

	return isMembersOpen && canFit;
};

export const useCanFitMemberList = (): boolean => {
	const [canFit, setCanFit] = React.useState(() => window.innerWidth >= MIN_WIDTH_FOR_MEMBERS);

	React.useEffect(() => {
		const checkWidth = () => {
			setCanFit(window.innerWidth >= MIN_WIDTH_FOR_MEMBERS);
		};

		window.addEventListener('resize', checkWidth);
		return () => window.removeEventListener('resize', checkWidth);
	}, []);

	return canFit;
};
