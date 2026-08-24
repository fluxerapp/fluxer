// SPDX-License-Identifier: AGPL-3.0-or-later

import MemberList from '@app/features/member/state/MemberList';
import {useEffect, useState} from 'react';

const MEMBER_LIST_FIT_QUERY = '(min-width: 1024px)';

interface UseMemberListVisibleOptions {
	channelId?: string | null;
	defaultHiddenForChannel?: boolean;
}

export const useCanFitMemberList = (): boolean => {
	const [canFit, setCanFit] = useState(() => window.matchMedia(MEMBER_LIST_FIT_QUERY).matches);
	useEffect(() => {
		const mediaQuery = window.matchMedia(MEMBER_LIST_FIT_QUERY);
		const handleChange = (event: MediaQueryListEvent | MediaQueryList) => {
			setCanFit(event.matches);
		};
		handleChange(mediaQuery);
		mediaQuery.addEventListener('change', handleChange);
		return () => mediaQuery.removeEventListener('change', handleChange);
	}, []);
	return canFit;
};

export const useMemberListVisible = (options: UseMemberListVisibleOptions = {}): boolean => {
	const canFit = useCanFitMemberList();
	return canFit && MemberList.isMembersVisible(options);
};
