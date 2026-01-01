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

import type React from 'react';
import {createContext, useContext} from 'react';

export interface ChannelListScrollbarContextValue {
	hasScrollbar: boolean;
}

export const ChannelListScrollbarContext = createContext<ChannelListScrollbarContextValue | null>(null);

export const useChannelListScrollbar = (): ChannelListScrollbarContextValue => {
	const context = useContext(ChannelListScrollbarContext);
	if (!context) {
		throw new Error('useChannelListScrollbar must be used within a ChannelListScrollbarProvider');
	}
	return context;
};

interface ChannelListScrollbarProviderProps {
	children: React.ReactNode;
	value: ChannelListScrollbarContextValue;
}

export const ChannelListScrollbarProvider: React.FC<ChannelListScrollbarProviderProps> = ({children, value}) => {
	return <ChannelListScrollbarContext.Provider value={value}>{children}</ChannelListScrollbarContext.Provider>;
};
