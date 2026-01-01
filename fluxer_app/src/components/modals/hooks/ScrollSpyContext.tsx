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
import {useScrollSpy} from './useScrollSpy';

export interface ScrollSpyContextValue {
	activeSectionId: string | null;
	scrollToSection: (sectionId: string) => void;
	sectionIds: ReadonlyArray<string>;
}

const ScrollSpyContext = React.createContext<ScrollSpyContextValue | null>(null);

export interface ScrollSpyProviderProps {
	sectionIds: ReadonlyArray<string>;
	containerRef: React.RefObject<HTMLElement | null>;
	offset?: number;
	children: React.ReactNode;
}

export const ScrollSpyProvider: React.FC<ScrollSpyProviderProps> = ({sectionIds, containerRef, offset, children}) => {
	const {activeSectionId, scrollToSection} = useScrollSpy({sectionIds, containerRef, offset});

	const contextValue = React.useMemo<ScrollSpyContextValue>(
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
	return React.useContext(ScrollSpyContext);
}
