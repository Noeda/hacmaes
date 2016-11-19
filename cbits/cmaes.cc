#include <vector>
#include "cmaes.h"
#include <stdint.h>

using namespace libcmaes;

extern "C" {
    void cmaes_optimize(double* initial, double sigma, int lambda, uint64_t num_coords, double (*evaluate)(double*, int*), void (*iterator)(void));
}

void cmaes_optimize(double* initial, double sigma, int lambda, uint64_t num_coords, double (*evaluate)(double*, int*), void (*iter)(void))
{
    FitFunc fit = [&evaluate](const double* params, const int N) {
        int dumb = 0;
        return evaluate(const_cast<double*>(params), &dumb);
    };

    ProgressFunc<CMAParameters<>,CMASolutions> pfunc = [&iter](const CMAParameters<> &cmaparams, const CMASolutions &cmasolutions)
    {
        iter();
        return 0;
    };

    std::vector<double> x0;
    for ( uint64_t i1 = 0; i1 < num_coords; ++i1 ) {
        x0.push_back(initial[i1]);
    }

    CMAParameters<> cmaparams(x0, sigma, lambda);
    cmaparams.set_algo(aCMAES);
    CMASolutions cmasols = cmaes<>(fit, cmaparams, pfunc);
    cmasols.sort_candidates();
    std::vector<double> out = cmasols.best_candidate().get_x();

    for ( uint64_t i1 = 0; i1 < num_coords; ++i1 ) {
        initial[i1] = out[i1];
    }
}

