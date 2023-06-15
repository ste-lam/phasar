/******************************************************************************
 * Copyright (c) 2020 Fabian Schiebel.
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of LICENSE.txt.
 *
 * Contributors:
 *     Fabian Schiebel and others
 *****************************************************************************/

#ifndef PHASAR_PHASARLLVM_DATAFLOWSOLVER_IFDSIDE_PROBLEMS_EXTENDEDTAINTANALYSIS_GENEDGEFUNCTION_H
#define PHASAR_PHASARLLVM_DATAFLOWSOLVER_IFDSIDE_PROBLEMS_EXTENDEDTAINTANALYSIS_GENEDGEFUNCTION_H

#include "phasar/DataFlow/IfdsIde/EdgeFunctionUtils.h"
#include "phasar/PhasarLLVM/DataFlow/IfdsIde/Problems/ExtendedTaintAnalysis/EdgeDomain.h"

namespace psr::XTaint {
using GenEdgeFunction = ConstantEdgeFunction<EdgeDomain>;
} // namespace psr::XTaint

#endif