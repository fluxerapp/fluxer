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
import type {GuildSplashCardAlignmentValue} from '~/Constants';

interface AuthLayoutContextType {
	setSplashUrl: (url: string | null) => void;
	setShowLogoSide: (show: boolean) => void;
	setSplashCardAlignment: React.Dispatch<React.SetStateAction<GuildSplashCardAlignmentValue>>;
}

export const AuthLayoutContext = React.createContext<AuthLayoutContextType | null>(null);

export const useAuthLayoutContext = () => {
	const context = React.useContext(AuthLayoutContext);
	if (!context) {
		throw new Error('useAuthLayoutContext must be used within AuthLayoutProvider');
	}
	return context;
};
