// SPDX-License-Identifier: AGPL-3.0-or-later

import {useScrollSpy} from '@app/features/user/hooks/useScrollSpy';
import React, {useContext, useMemo} from 'react';

export interface ScrollSpyContextValue {
	activeSectionId: string | null;
	scrollToSection: (sectionId: string) => boolean;
	sectionIds: ReadonlyArray<string>;
}

const ScrollSpyContext = React.createContext<ScrollSpyContextValue | null>(null);

export interface ScrollSpyProviderProps {
	sectionIds: ReadonlyArray<string>;
	container: HTMLElement | null;
	children: React.ReactNode;
}

export const ScrollSpyProvider: React.FC<ScrollSpyProviderProps> = ({sectionIds, container, children}) => {
	const {activeSectionId, scrollToSection} = useScrollSpy({sectionIds, container});
	const contextValue = useMemo<ScrollSpyContextValue>(
		() => ({
			activeSectionId,
			scrollToSection,
			sectionIds,
		}),
		[activeSectionId, scrollToSection, sectionIds],
	);
	return <ScrollSpyContext.Provider value={contextValue}>{children}</ScrollSpyContext.Provider>;
};

export function useScrollSpyContext(): ScrollSpyContextValue | null {
	return useContext(ScrollSpyContext);
}
