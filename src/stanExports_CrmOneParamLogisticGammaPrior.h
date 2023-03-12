// Generated by rstantools.  Do not edit by hand.

/*
    trialr is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    trialr is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with trialr.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_CrmOneParamLogisticGammaPrior_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_CrmOneParamLogisticGammaPrior");
    reader.add_event(105, 103, "end", "model_CrmOneParamLogisticGammaPrior");
    return reader;
}
template <typename T3__, typename T4__, typename T5__, typename T6__>
typename boost::math::tools::promote_args<T3__, T4__, T5__, T6__>::type
log_joint_pdf(const int& num_patients,
                  const std::vector<int>& tox,
                  const std::vector<int>& doses,
                  const std::vector<T3__>& weights,
                  const std::vector<T4__>& codified_doses,
                  const T5__& a0,
                  const T6__& beta, std::ostream* pstream__) {
    typedef typename boost::math::tools::promote_args<T3__, T4__, T5__, T6__>::type local_scalar_t__;
    typedef local_scalar_t__ fun_return_scalar_t__;
    const static bool propto__ = true;
    (void) propto__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
    int current_statement_begin__ = -1;
    try {
        {
        current_statement_begin__ = 22;
        local_scalar_t__ p(DUMMY_VAR__);
        (void) p;  // dummy to suppress unused var warning
        stan::math::initialize(p, DUMMY_VAR__);
        stan::math::fill(p, DUMMY_VAR__);
        current_statement_begin__ = 23;
        stan::math::assign(p, 0);
        current_statement_begin__ = 24;
        for (int j = 1; j <= num_patients; ++j) {
            {
            current_statement_begin__ = 25;
            local_scalar_t__ prob_tox(DUMMY_VAR__);
            (void) prob_tox;  // dummy to suppress unused var warning
            stan::math::initialize(prob_tox, DUMMY_VAR__);
            stan::math::fill(prob_tox, DUMMY_VAR__);
            current_statement_begin__ = 26;
            local_scalar_t__ p_j(DUMMY_VAR__);
            (void) p_j;  // dummy to suppress unused var warning
            stan::math::initialize(p_j, DUMMY_VAR__);
            stan::math::fill(p_j, DUMMY_VAR__);
            current_statement_begin__ = 27;
            stan::math::assign(prob_tox, inv_logit((a0 + (beta * get_base1(codified_doses, get_base1(doses, j, "doses", 1), "codified_doses", 1)))));
            current_statement_begin__ = 28;
            stan::math::assign(p_j, (pow((get_base1(weights, j, "weights", 1) * prob_tox), get_base1(tox, j, "tox", 1)) * pow((1 - (get_base1(weights, j, "weights", 1) * prob_tox)), (1 - get_base1(tox, j, "tox", 1)))));
            current_statement_begin__ = 30;
            stan::math::assign(p, (p + stan::math::log(p_j)));
            }
        }
        current_statement_begin__ = 32;
        return stan::math::promote_scalar<fun_return_scalar_t__>(p);
        }
    } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
        // Next line prevents compiler griping about no return
        throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
    }
}
struct log_joint_pdf_functor__ {
    template <typename T3__, typename T4__, typename T5__, typename T6__>
        typename boost::math::tools::promote_args<T3__, T4__, T5__, T6__>::type
    operator()(const int& num_patients,
                  const std::vector<int>& tox,
                  const std::vector<int>& doses,
                  const std::vector<T3__>& weights,
                  const std::vector<T4__>& codified_doses,
                  const T5__& a0,
                  const T6__& beta, std::ostream* pstream__) const {
        return log_joint_pdf(num_patients, tox, doses, weights, codified_doses, a0, beta, pstream__);
    }
};
#include <stan_meta_header.hpp>
class model_CrmOneParamLogisticGammaPrior
  : public stan::model::model_base_crtp<model_CrmOneParamLogisticGammaPrior> {
private:
        double beta_shape;
        double beta_inverse_scale;
        int num_doses;
        std::vector<double> skeleton;
        double a0;
        int num_patients;
        std::vector<int> tox;
        std::vector<int> doses;
        std::vector<double> weights;
        std::vector<double> codified_doses;
public:
    model_CrmOneParamLogisticGammaPrior(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_CrmOneParamLogisticGammaPrior(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_CrmOneParamLogisticGammaPrior_namespace::model_CrmOneParamLogisticGammaPrior";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 38;
            context__.validate_dims("data initialization", "beta_shape", "double", context__.to_vec());
            beta_shape = double(0);
            vals_r__ = context__.vals_r("beta_shape");
            pos__ = 0;
            beta_shape = vals_r__[pos__++];
            check_greater_or_equal(function__, "beta_shape", beta_shape, 0);
            current_statement_begin__ = 39;
            context__.validate_dims("data initialization", "beta_inverse_scale", "double", context__.to_vec());
            beta_inverse_scale = double(0);
            vals_r__ = context__.vals_r("beta_inverse_scale");
            pos__ = 0;
            beta_inverse_scale = vals_r__[pos__++];
            check_greater_or_equal(function__, "beta_inverse_scale", beta_inverse_scale, 0);
            current_statement_begin__ = 42;
            context__.validate_dims("data initialization", "num_doses", "int", context__.to_vec());
            num_doses = int(0);
            vals_i__ = context__.vals_i("num_doses");
            pos__ = 0;
            num_doses = vals_i__[pos__++];
            check_greater_or_equal(function__, "num_doses", num_doses, 1);
            current_statement_begin__ = 46;
            validate_non_negative_index("skeleton", "num_doses", num_doses);
            context__.validate_dims("data initialization", "skeleton", "double", context__.to_vec(num_doses));
            skeleton = std::vector<double>(num_doses, double(0));
            vals_r__ = context__.vals_r("skeleton");
            pos__ = 0;
            size_t skeleton_k_0_max__ = num_doses;
            for (size_t k_0__ = 0; k_0__ < skeleton_k_0_max__; ++k_0__) {
                skeleton[k_0__] = vals_r__[pos__++];
            }
            size_t skeleton_i_0_max__ = num_doses;
            for (size_t i_0__ = 0; i_0__ < skeleton_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "skeleton[i_0__]", skeleton[i_0__], 0);
                check_less_or_equal(function__, "skeleton[i_0__]", skeleton[i_0__], 1);
            }
            current_statement_begin__ = 48;
            context__.validate_dims("data initialization", "a0", "double", context__.to_vec());
            a0 = double(0);
            vals_r__ = context__.vals_r("a0");
            pos__ = 0;
            a0 = vals_r__[pos__++];
            current_statement_begin__ = 51;
            context__.validate_dims("data initialization", "num_patients", "int", context__.to_vec());
            num_patients = int(0);
            vals_i__ = context__.vals_i("num_patients");
            pos__ = 0;
            num_patients = vals_i__[pos__++];
            check_greater_or_equal(function__, "num_patients", num_patients, 0);
            current_statement_begin__ = 53;
            validate_non_negative_index("tox", "num_patients", num_patients);
            context__.validate_dims("data initialization", "tox", "int", context__.to_vec(num_patients));
            tox = std::vector<int>(num_patients, int(0));
            vals_i__ = context__.vals_i("tox");
            pos__ = 0;
            size_t tox_k_0_max__ = num_patients;
            for (size_t k_0__ = 0; k_0__ < tox_k_0_max__; ++k_0__) {
                tox[k_0__] = vals_i__[pos__++];
            }
            size_t tox_i_0_max__ = num_patients;
            for (size_t i_0__ = 0; i_0__ < tox_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "tox[i_0__]", tox[i_0__], 0);
                check_less_or_equal(function__, "tox[i_0__]", tox[i_0__], 1);
            }
            current_statement_begin__ = 57;
            validate_non_negative_index("doses", "num_patients", num_patients);
            context__.validate_dims("data initialization", "doses", "int", context__.to_vec(num_patients));
            doses = std::vector<int>(num_patients, int(0));
            vals_i__ = context__.vals_i("doses");
            pos__ = 0;
            size_t doses_k_0_max__ = num_patients;
            for (size_t k_0__ = 0; k_0__ < doses_k_0_max__; ++k_0__) {
                doses[k_0__] = vals_i__[pos__++];
            }
            size_t doses_i_0_max__ = num_patients;
            for (size_t i_0__ = 0; i_0__ < doses_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "doses[i_0__]", doses[i_0__], 1);
                check_less_or_equal(function__, "doses[i_0__]", doses[i_0__], num_doses);
            }
            current_statement_begin__ = 61;
            validate_non_negative_index("weights", "num_patients", num_patients);
            context__.validate_dims("data initialization", "weights", "double", context__.to_vec(num_patients));
            weights = std::vector<double>(num_patients, double(0));
            vals_r__ = context__.vals_r("weights");
            pos__ = 0;
            size_t weights_k_0_max__ = num_patients;
            for (size_t k_0__ = 0; k_0__ < weights_k_0_max__; ++k_0__) {
                weights[k_0__] = vals_r__[pos__++];
            }
            // initialize transformed data variables
            current_statement_begin__ = 69;
            validate_non_negative_index("codified_doses", "num_doses", num_doses);
            codified_doses = std::vector<double>(num_doses, double(0));
            stan::math::fill(codified_doses, DUMMY_VAR__);
            // execute transformed data statements
            current_statement_begin__ = 70;
            for (int i = 1; i <= num_doses; ++i) {
                current_statement_begin__ = 71;
                stan::model::assign(codified_doses, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            ((logit(get_base1(skeleton, i, "skeleton", 1)) - a0) / (beta_shape / beta_inverse_scale)), 
                            "assigning variable codified_doses");
            }
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 77;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_CrmOneParamLogisticGammaPrior() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 77;
        if (!(context__.contains_r("beta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable beta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("beta");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "beta", "double", context__.to_vec());
        double beta(0);
        beta = vals_r__[pos__++];
        try {
            writer__.scalar_unconstrain(beta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable beta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 77;
            local_scalar_t__ beta;
            (void) beta;  // dummy to suppress unused var warning
            if (jacobian__)
                beta = in__.scalar_constrain(lp__);
            else
                beta = in__.scalar_constrain();
            // transformed parameters
            current_statement_begin__ = 82;
            validate_non_negative_index("prob_tox", "num_doses", num_doses);
            std::vector<local_scalar_t__> prob_tox(num_doses, local_scalar_t__(0));
            stan::math::initialize(prob_tox, DUMMY_VAR__);
            stan::math::fill(prob_tox, DUMMY_VAR__);
            // transformed parameters block statements
            current_statement_begin__ = 83;
            for (int i = 1; i <= num_doses; ++i) {
                current_statement_begin__ = 84;
                stan::model::assign(prob_tox, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            inv_logit((a0 + (beta * get_base1(codified_doses, i, "codified_doses", 1)))), 
                            "assigning variable prob_tox");
            }
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 82;
            size_t prob_tox_k_0_max__ = num_doses;
            for (size_t k_0__ = 0; k_0__ < prob_tox_k_0_max__; ++k_0__) {
                if (stan::math::is_uninitialized(prob_tox[k_0__])) {
                    std::stringstream msg__;
                    msg__ << "Undefined transformed parameter: prob_tox" << "[" << k_0__ << "]";
                    stan::lang::rethrow_located(std::runtime_error(std::string("Error initializing variable prob_tox: ") + msg__.str()), current_statement_begin__, prog_reader__());
                }
            }
            size_t prob_tox_i_0_max__ = num_doses;
            for (size_t i_0__ = 0; i_0__ < prob_tox_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "prob_tox[i_0__]", prob_tox[i_0__], 0);
                check_less_or_equal(function__, "prob_tox[i_0__]", prob_tox[i_0__], 1);
            }
            // model body
            current_statement_begin__ = 89;
            lp_accum__.add(gamma_log(beta, beta_shape, beta_inverse_scale));
            current_statement_begin__ = 90;
            lp_accum__.add(log_joint_pdf(num_patients, tox, doses, weights, codified_doses, a0, beta, pstream__));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("beta");
        names__.push_back("prob_tox");
        names__.push_back("log_lik");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(num_doses);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(num_patients);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_CrmOneParamLogisticGammaPrior_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        double beta = in__.scalar_constrain();
        vars__.push_back(beta);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            // declare and define transformed parameters
            current_statement_begin__ = 82;
            validate_non_negative_index("prob_tox", "num_doses", num_doses);
            std::vector<double> prob_tox(num_doses, double(0));
            stan::math::initialize(prob_tox, DUMMY_VAR__);
            stan::math::fill(prob_tox, DUMMY_VAR__);
            // do transformed parameters statements
            current_statement_begin__ = 83;
            for (int i = 1; i <= num_doses; ++i) {
                current_statement_begin__ = 84;
                stan::model::assign(prob_tox, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            inv_logit((a0 + (beta * get_base1(codified_doses, i, "codified_doses", 1)))), 
                            "assigning variable prob_tox");
            }
            if (!include_gqs__ && !include_tparams__) return;
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 82;
            size_t prob_tox_i_0_max__ = num_doses;
            for (size_t i_0__ = 0; i_0__ < prob_tox_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "prob_tox[i_0__]", prob_tox[i_0__], 0);
                check_less_or_equal(function__, "prob_tox[i_0__]", prob_tox[i_0__], 1);
            }
            // write transformed parameters
            if (include_tparams__) {
                size_t prob_tox_k_0_max__ = num_doses;
                for (size_t k_0__ = 0; k_0__ < prob_tox_k_0_max__; ++k_0__) {
                    vars__.push_back(prob_tox[k_0__]);
                }
            }
            if (!include_gqs__) return;
            // declare and define generated quantities
            current_statement_begin__ = 95;
            validate_non_negative_index("log_lik", "num_patients", num_patients);
            Eigen::Matrix<double, Eigen::Dynamic, 1> log_lik(num_patients);
            stan::math::initialize(log_lik, DUMMY_VAR__);
            stan::math::fill(log_lik, DUMMY_VAR__);
            // generated quantities statements
            current_statement_begin__ = 96;
            for (int j = 1; j <= num_patients; ++j) {
                {
                current_statement_begin__ = 97;
                local_scalar_t__ p_j(DUMMY_VAR__);
                (void) p_j;  // dummy to suppress unused var warning
                stan::math::initialize(p_j, DUMMY_VAR__);
                stan::math::fill(p_j, DUMMY_VAR__);
                current_statement_begin__ = 98;
                stan::math::assign(p_j, inv_logit((a0 + (beta * get_base1(codified_doses, get_base1(doses, j, "doses", 1), "codified_doses", 1)))));
                current_statement_begin__ = 99;
                stan::model::assign(log_lik, 
                            stan::model::cons_list(stan::model::index_uni(j), stan::model::nil_index_list()), 
                            stan::math::log((pow((get_base1(weights, j, "weights", 1) * p_j), get_base1(tox, j, "tox", 1)) * pow((1 - (get_base1(weights, j, "weights", 1) * p_j)), (1 - get_base1(tox, j, "tox", 1))))), 
                            "assigning variable log_lik");
                }
            }
            // validate, write generated quantities
            current_statement_begin__ = 95;
            size_t log_lik_j_1_max__ = num_patients;
            for (size_t j_1__ = 0; j_1__ < log_lik_j_1_max__; ++j_1__) {
                vars__.push_back(log_lik(j_1__));
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_CrmOneParamLogisticGammaPrior";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "beta";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t prob_tox_k_0_max__ = num_doses;
            for (size_t k_0__ = 0; k_0__ < prob_tox_k_0_max__; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "prob_tox" << '.' << k_0__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
        size_t log_lik_j_1_max__ = num_patients;
        for (size_t j_1__ = 0; j_1__ < log_lik_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "log_lik" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "beta";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t prob_tox_k_0_max__ = num_doses;
            for (size_t k_0__ = 0; k_0__ < prob_tox_k_0_max__; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "prob_tox" << '.' << k_0__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
        size_t log_lik_j_1_max__ = num_patients;
        for (size_t j_1__ = 0; j_1__ < log_lik_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "log_lik" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
    }
}; // model
}  // namespace
typedef model_CrmOneParamLogisticGammaPrior_namespace::model_CrmOneParamLogisticGammaPrior stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif
