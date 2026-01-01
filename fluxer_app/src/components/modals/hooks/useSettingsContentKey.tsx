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

interface SettingsContentKeyContextValue {
	contentKey: string | null;
	setContentKey: (key: string | null) => void;
	resetContentKey: () => void;
}

const SettingsContentKeyContext = React.createContext<SettingsContentKeyContextValue | null>(null);

export const SettingsContentKeyProvider: React.FC<{children: React.ReactNode}> = ({children}) => {
	const [contentKey, setContentKey] = React.useState<string | null>(null);

	const handleSetContentKey = React.useCallback((key: string | null) => {
		setContentKey(key);
	}, []);

	const resetContentKey = React.useCallback(() => {
		setContentKey(null);
	}, []);

	const value = React.useMemo(
		() => ({
			contentKey,
			setContentKey: handleSetContentKey,
			resetContentKey,
		}),
		[contentKey, handleSetContentKey, resetContentKey],
	);

	return <SettingsContentKeyContext.Provider value={value}>{children}</SettingsContentKeyContext.Provider>;
};

export const useSettingsContentKey = (): SettingsContentKeyContextValue => {
	const context = React.useContext(SettingsContentKeyContext);

	if (!context) {
		return {
			contentKey: null,
			setContentKey: () => {},
			resetContentKey: () => {},
		};
	}

	return context;
};
