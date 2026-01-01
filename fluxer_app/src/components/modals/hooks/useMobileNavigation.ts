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
import type {UserSettingsTabType} from '../utils/settingsConstants';

interface NavigationStackItem<T = UserSettingsTabType> {
	tab: T;
	title: string;
}

export interface MobileNavigationState<T = UserSettingsTabType> {
	navigationStack: Array<NavigationStackItem<T>>;
	direction: 'forward' | 'backward';
	currentView: NavigationStackItem<T> | undefined;
	isRootView: boolean;
	navigateTo: (tab: T, title: string) => void;
	navigateBack: () => void;
	resetToRoot: () => void;
}

type NavigationAction<T = UserSettingsTabType> =
	| {type: 'NAVIGATE_FORWARD'; payload: NavigationStackItem<T>}
	| {type: 'NAVIGATE_BACK'}
	| {type: 'RESET_TO_ROOT'};

interface NavigationReducerState<T = UserSettingsTabType> {
	stack: Array<NavigationStackItem<T>>;
	direction: 'forward' | 'backward';
}

function navigationReducer<T>(
	state: NavigationReducerState<T>,
	action: NavigationAction<T>,
): NavigationReducerState<T> {
	switch (action.type) {
		case 'NAVIGATE_FORWARD':
			return {
				stack: [...state.stack, action.payload],
				direction: 'forward',
			};

		case 'NAVIGATE_BACK':
			if (state.stack.length === 0) {
				return state;
			}
			return {
				stack: state.stack.slice(0, -1),
				direction: 'backward',
			};

		case 'RESET_TO_ROOT':
			return {
				stack: [],
				direction: 'backward',
			};

		default:
			return state;
	}
}

export const useMobileNavigation = <T = UserSettingsTabType>(initialTab?: {
	tab: T;
	title: string;
}): MobileNavigationState<T> => {
	const initialState: NavigationReducerState<T> = {
		stack: initialTab ? [initialTab] : [],
		direction: 'forward',
	};
	const [state, dispatch] = React.useReducer(navigationReducer, initialState);

	const navigateTo = React.useCallback((tab: T, title: string) => {
		dispatch({type: 'NAVIGATE_FORWARD', payload: {tab, title}});
	}, []);

	const navigateBack = React.useCallback(() => {
		dispatch({type: 'NAVIGATE_BACK'});
	}, []);

	const resetToRoot = React.useCallback(() => {
		dispatch({type: 'RESET_TO_ROOT'});
	}, []);

	const currentView = state.stack[state.stack.length - 1];
	const isRootView = state.stack.length === 0;

	return {
		navigationStack: state.stack,
		direction: state.direction,
		currentView,
		isRootView,
		navigateTo,
		navigateBack,
		resetToRoot,
	};
};
