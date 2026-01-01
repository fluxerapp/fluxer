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

import {clsx} from 'clsx';
import * as React from 'react';
import styles from './FocusRing.module.css';
import FocusRingContext, {FocusRingContextManager} from './FocusRingContext';
import FocusRingManager from './FocusRingManager';

interface FocusRingScopeProps {
	containerRef: React.RefObject<Element | null>;
	children: React.ReactNode;
}

export default function FocusRingScope(props: FocusRingScopeProps) {
	const {containerRef, children} = props;
	const manager = React.useRef(new FocusRingContextManager());

	React.useEffect(() => {
		manager.current.setContainer(containerRef.current);
	}, [containerRef]);

	return (
		<FocusRingContext.Provider value={manager.current}>
			{children}
			<Ring />
		</FocusRingContext.Provider>
	);
}

function Ring() {
	const ringContext = React.useContext(FocusRingContext);
	const [, forceUpdate] = React.useReducer((x: number) => x + 1, 0);

	React.useEffect(() => {
		ringContext.invalidate = () => forceUpdate();
		return () => {
			ringContext.invalidate = () => null;
		};
	}, [ringContext]);

	if (!FocusRingManager.ringsEnabled || !ringContext.visible) return null;

	return <div className={clsx(styles.focusRing, ringContext.className)} style={ringContext.getStyle()} />;
}
