// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/focus_ring/FocusRing.module.css';
import FocusRingContext, {FocusRingContextManager} from '@app/features/ui/focus_ring/FocusRingContext';
import FocusRingManager from '@app/features/ui/focus_ring/FocusRingManager';
import {clsx} from 'clsx';
import {Observer} from 'mobx-react-lite';
import type * as React from 'react';
import {useContext, useEffect, useReducer, useRef} from 'react';

interface FocusRingScopeProps {
	containerRef: React.RefObject<Element | null>;
	children: React.ReactNode;
}

export default function FocusRingScope(props: FocusRingScopeProps) {
	const {containerRef, children} = props;
	const manager = useRef(new FocusRingContextManager());
	useEffect(() => {
		manager.current.setContainer(containerRef.current);
	}, [containerRef]);
	return (
		<FocusRingContext.Provider value={manager.current}>
			{children}
			<Ring data-flx="ui.focus-ring.focus-ring-scope.ring" />
		</FocusRingContext.Provider>
	);
}

function Ring() {
	const ringContext = useContext(FocusRingContext);
	const [, forceUpdate] = useReducer((x: number) => x + 1, 0);
	const targetElement = ringContext.targetElement;
	useEffect(() => {
		ringContext.invalidate = () => forceUpdate();
		return () => {
			ringContext.invalidate = () => null;
		};
	}, [ringContext]);
	useEffect(() => {
		if (targetElement == null || typeof ResizeObserver === 'undefined') return;
		let rafId: number | null = null;
		const scheduleInvalidate = () => {
			if (rafId != null) return;
			rafId = requestAnimationFrame(() => {
				rafId = null;
				ringContext.invalidate();
			});
		};
		const resizeObserver = new ResizeObserver(scheduleInvalidate);
		resizeObserver.observe(targetElement);
		return () => {
			if (rafId != null) {
				cancelAnimationFrame(rafId);
			}
			resizeObserver.disconnect();
		};
	}, [ringContext, targetElement]);
	return (
		<Observer data-flx="ui.focus-ring.focus-ring-scope.ring.observer">
			{() => {
				if (!FocusRingManager.ringsEnabled || !ringContext.visible) return null;
				return (
					<div
						className={clsx(styles.focusRing, ringContext.className)}
						style={ringContext.getStyle()}
						data-flx="ui.focus-ring.focus-ring-scope.ring.focus-ring"
					/>
				);
			}}
		</Observer>
	);
}
